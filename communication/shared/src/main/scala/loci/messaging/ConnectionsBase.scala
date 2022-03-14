package loci
package messaging

import communicator._
import compatibility.jdkCollectionConverters._

import java.util.concurrent.{ConcurrentHashMap, ConcurrentLinkedQueue}
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.locks.ReentrantLock

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConnectionsBase {
  type Protocol = ProtocolCommon with Bidirectional
}

case object IdentityMessage {
  implicit val message =
    messaging.Message.Method(IdentityMessage -> "Loci/Identity")

  final val Identity = "Identity"
}
trait ConnectionsBase[R, M] {
  protected def terminatedException =
    new ConnectionException("remote connection terminated")

  protected class BaseState {
    private val running = new AtomicBoolean(false)
    private val terminated = new AtomicBoolean(false)

    def run() = running.set(true)
    def terminate() = terminated.set(true)
    def isRunning = running.get && !terminated.get
    def isTerminated = terminated.get

    private[ConnectionsBase] val messages = mutable.ListBuffer.empty[(R, M)]
    private[ConnectionsBase] val listeners = mutable.ListBuffer.empty[Listening]
    private[ConnectionsBase] val remotes = new ConcurrentLinkedQueue[R]
    private[ConnectionsBase] val connections =
      new ConcurrentHashMap[R, Connection[ConnectionsBase.Protocol]]
  }

  protected val state: BaseState

  private val doRemoteJoined = Notice.Stream[R]

  private val doRemoteLeft = Notice.Stream[R]

  private val doTerminated = Notice.Steady[List[R]]

  private val doReceive = Notice.Stream[(R, M)]

  val remoteJoined: Notice.Stream[R] = doRemoteJoined.notice

  val remoteLeft: Notice.Stream[R] = doRemoteLeft.notice

  val terminated: Notice.Steady[List[R]] = doTerminated.notice

  val receive: Notice.Stream[(R, M)] = doReceive.notice

  private var identity: String = null

  def remotes: List[R] = state.remotes.asScala.toList

  def isRunning: Boolean = state.isRunning

  def isTerminated: Boolean = state.isTerminated

  def isConnected(remote: R): Boolean =
    state.connections containsKey remote

  def run(): Unit =
    sync {
      if (!state.isTerminated && !state.isRunning) {
        logging.trace("connection system started")
        identity = java.util.UUID.randomUUID().toString

        state.messages foreach doReceive.fire
        state.messages.clear()
        state.run()
      }
    }

  def terminate(): Unit =
    sync {
      if (!state.isTerminated) {
        logging.trace("connection system terminated")

        val remotes = state.remotes.asScala.toList
        val connections = state.connections.asScala.toSeq
        val listeners = state.listeners.toSeq

        state.terminate()

        afterSync {
          connections foreach { case (_, connection) => connection.close() }
          listeners foreach { _.stopListening() }
          doTerminated.set(remotes)

          state.remotes.clear()
          state.connections.clear()
          state.listeners.clear()
        }
      }
    }

  def disconnect(remote: R): Unit =
    if (!state.isTerminated)
      Option(state.connections get remote) foreach { _.close() }

  def send(remote: R, message: M): Unit =
    if (!state.isTerminated)
      state.connections get remote match {
        case null =>
          logging.warn(s"message not sent to unconnected remote $remote: $message")
        case connection =>
          connection.send(serializeMessage(message))
      }
    else
      logging.warn(s"message not sent after connection system shutdown to remote $remote: $message")

  private def doBufferedReceive(remote: R, message: M): Unit =
    if (!state.isTerminated) {
      if (state.isRunning)
        doReceive.fire(remote -> message)
      else
        sync {
          if (state.isRunning)
            doReceive.fire(remote -> message)
          else
            state.messages += remote -> message
        }
    }

  private val syncLock = new ReentrantLock

  private val syncHandlers = new ThreadLocal[mutable.ListBuffer[() => Unit]] {
    override def initialValue = mutable.ListBuffer.empty
  }

  protected def sync[T](body: => T): T = {
    syncLock.lock
    try body
    finally {
      syncLock.unlock
      if (syncLock.getHoldCount == 0) {
        val handlers = syncHandlers.get
        if (handlers.nonEmpty) {
          val result = handlers.result()
          handlers.clear()
          result foreach { _.apply() }
        }
      }
    }
  }

  protected def afterSync[T](handler: => T): Unit =
    if (syncLock.getHoldCount == 0)
      throw new UnsupportedOperationException(
        "current thread does not hold the synchronization lock")
    else
      syncHandlers.get += { () => handler }


  protected def deserializeMessage(message: MessageBuffer): Try[M]

  protected def serializeMessage(message: M): MessageBuffer


  protected def addListening(listening: Listening): Try[Unit] =
    sync {
      if (!isTerminated) {
        state.listeners += listening
        Success(())
      }
      else
        Failure(terminatedException)
    }

  private def sendIdentityMessage(connection: Connection[ConnectionsBase.Protocol]) = {
    val message = messaging.Message[IdentityMessage.type](IdentityMessage,
      Map(IdentityMessage.Identity -> Seq(identity)), MessageBuffer.empty)
    connection.send(messaging.Message.serialize[IdentityMessage.type](message))
  }

  /**
   * Performs the exchange of identities between peers
   *
   * This method needs to be called, after the underlying network connection has been established, but before the
   * RemoteRef is created.
   *
   * The connector sends an Identity message to the listener first. The listener answers with its own identity. There
   * are no other parts of the protocol.
   *
   * @param connection Connection to remote peer
   * @param handler Function which is called after successfull exchage of identities
   * @param is_listener Flag whether we act as listener or connector
   */
  protected def exchangeIdentities(connection: Connection[ConnectionsBase.Protocol], handler: String => Unit, is_listener: Boolean): Unit = {
    var remote_identity: String = null

    var receive_handler: Notice[_]= null

    receive_handler = connection.receive foreach {
      message => {
        messaging.Message.deserialize[IdentityMessage.type](message) foreach { received_message =>
          val messaging.Message(_, properties, _) = received_message
          remote_identity = properties get IdentityMessage.Identity match {
            case Some(Seq(identity)) => {
              if (receive_handler != null)
                receive_handler.remove()
              if (is_listener) {
                sendIdentityMessage(connection)
              }
              identity
            }
            case _ => null
          }

          handler(remote_identity)
        }
      }
    }

    if (!is_listener) {
      sendIdentityMessage(connection)
    }
  }

  protected def addConnection(
      remote: R, connection: Connection[ConnectionsBase.Protocol]): Try[Unit] =
    sync {
      if (!isTerminated) {
        logging.info(s"established connection to remote $remote")

        state.connections.put(remote, connection)
        state.remotes.add(remote)

        afterSync { doRemoteJoined.fire(remote) }

        var receiveHandler: Notice[_] = null
        var closedHandler: Notice[_] = null

        receiveHandler = connection.receive foreach {
          deserializeMessage(_) map { doBufferedReceive(remote, _) }
        }

        closedHandler = connection.closed foreach { _ =>
          if (receiveHandler != null)
            receiveHandler.remove()
          if (closedHandler != null)
            closedHandler.remove()
          removeConnection(remote)
        }

        if (!connection.open) {
          receiveHandler.remove()
          closedHandler.remove()
          removeConnection(remote)
          Failure(terminatedException)
        }
        else
          Success(())
      }
      else
        Failure(terminatedException)
    }

  protected def removeConnection(remote: R): Unit =
    sync {
      if (state.connections containsKey remote) {
        logging.info(s"terminated connection to remote $remote")

        state.remotes.remove(remote)
        state.connections.remove(remote)
        afterSync { doRemoteLeft.fire(remote) }
      }
    }
}
