package loci
package language
package transmitter

import communicator.Connector
import messaging.ConnectionsBase

object RemoteGateway {
  trait Default { this: embedding.Gateway.type =>
    implicit class DefaultMultipleGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Multiple]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remotes
      def connect(connector: Connector[ConnectionsBase.Protocol]) =
        gateway.connectRemote(connector)
    }

    implicit class DefaultOptionalGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Optional]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remote
      def connect(connector: Connector[ConnectionsBase.Protocol]) =
        gateway.connectRemote(connector)
    }

    implicit class DefaultSingleGateway[R](gateway: Gateway[R])(implicit
        ev: Connection[R, Single]) extends RemoteGateway {
      val joined = gateway.remoteJoined
      val left = gateway.remoteLeft
      def connected = gateway.remote
      def connect(connector: Connector[ConnectionsBase.Protocol]) =
        gateway.connectRemote(connector)
    }
  }

  sealed trait Access {
    implicit class MultipleGatewayAccess[R](gateway: Gateway[R])(implicit
        ev: Connection[R, _]) {

      def cache[B <: AnyRef](id: Any)(body: => B): B = ev.cache(id, body)
      val remoteJoined: Notice.Stream[Remote[R]] = ev.remoteJoined
      val remoteLeft: Notice.Stream[Remote[R]] = ev.remoteLeft
      def remotes: Seq[Remote[R]] = ev.remoteReferences
      def connectRemote(connector: Connector[ConnectionsBase.Protocol]): Unit =
        ev.remoteConnect(connector)
    }
  }
}

trait RemoteGateway extends RemoteGateway.Access {
  implicit class OptionalGatewayAccess[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Optional])
    extends MultipleGatewayAccess(gateway)(ev) {

    def remote: Option[Remote[R]] = ev.remoteReferences.headOption
  }

  implicit class SingleGatewayAccess[R](gateway: Gateway[R])(implicit
      ev: Connection[R, Single])
    extends MultipleGatewayAccess(gateway)(ev) {

    def remote: Remote[R] = ev.remoteReferences.head
  }
}
