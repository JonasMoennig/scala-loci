package loci
package communicator
package ws.akka

import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.ws.{InvalidUpgradeResponse, ValidUpgrade, WebSocketRequest}
import akka.stream.{ConnectionException, Materializer}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

private object WSConnector {
  locally(WSConnector)

  def apply[P <: WS: WSProtocolFactory](
      http: HttpExt,
      webSocketRequest: WebSocketRequest,
      properties: WS.Properties)(implicit
      materializer: Materializer) =
    new WSConnector[P](
      http, properties, webSocketRequest, Function const { })

  def apply[P <: WS: WSProtocolFactory](
      webSocketRequest: WebSocketRequest,
      properties: WS.Properties) = {
    implicit val (actorSystem, actorMaterializer) = WSActorSystem.retrieve()
    new WSConnector[P](
      Http(), properties, webSocketRequest, {
        case Success(connection) =>
          connection.closed foreach { _ => WSActorSystem.release() }
        case _ =>
          WSActorSystem.release()
      })
  }

  class WSConnector[P <: WS: WSProtocolFactory](
    http: HttpExt,
    properties: WS.Properties,
    webSocketRequest: WebSocketRequest,
    webSocketConnectionEstablished: Try[Connection[P]] => Unit)(implicit
    materializer: Materializer)
      extends Connector[P] {

    def connect(connectionEstablished: Connected[P]) = {
      val protocolPromise = Promise[P]

      def connected(connection: Try[Connection[P]]) = {
        webSocketConnectionEstablished(connection)
        connectionEstablished.set(connection)
      }

      val (future, _) =
        try http.singleWebSocketRequest(
          webSocketRequest,
          WSHandler.handleWebSocket(protocolPromise.future, properties, connected))
        catch {
          case NonFatal(exception) => (Future failed exception, ())
        }

      future onComplete {
        case Success(ValidUpgrade(response, _)) =>
          val uri = webSocketRequest.uri

          val WSSecurityProperties(isAuthenticated, isProtected, isEncrypted, certificates) =
            WSSecurityProperties(webSocketRequest, response, authenticated = false)

          implicitly[WSProtocolFactory[P]] make (
              uri.toString,
              Some(uri.authority.host.address), Some(uri.effectivePort),
              WSConnector.this, isAuthenticated, isEncrypted, isProtected,
              Some(Right(response)), Left(certificates))  match {
            case Failure(exception) =>
              connected(Failure(exception))

            case Success(ws) =>
              protocolPromise.success(ws)
          }

        case Success(InvalidUpgradeResponse(_, cause)) =>
          connected(Failure(new ConnectionException(cause)))

        case Failure(exception) =>
          connected(Failure(exception))
      }
    }
  }
}
