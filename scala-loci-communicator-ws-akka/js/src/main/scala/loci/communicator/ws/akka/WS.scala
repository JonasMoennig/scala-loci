package loci
package communicator
package ws.akka

import akka.actor.ActorSystem
import akka.http.scaladsl.HttpExt
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.ws.WebSocketRequest
import akka.http.scaladsl.server.Route
import akka.stream.Materializer

import scala.annotation.compileTimeOnly
import scala.concurrent.duration._

trait WS extends
    Protocol with
    SetupInfo with
    SecurityInfo with
    SymmetryInfo with Bidirectional {
  val url: String
  val host: Option[String]
  val port: Option[Int]

  override def toString = s"WS($url, $host, $port)"
}

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
object WS extends WSSetupFactory {
  def unapply(ws: WS) = Some((ws.url, ws.host, ws.port))

  case class Properties(
    heartbeatDelay: FiniteDuration = 3.seconds,
    heartbeatTimeout: FiniteDuration = 10.seconds)

  private[akka] def ??? =
    sys.error("Akka WebSocket communicator only available on the JVM")

  def apply(
    http: HttpExt, port: Int)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer): Listener[WS] =
      { locally(actorRefFactory); locally(materializer); ??? }
  def apply(
    http: HttpExt, port: Int, interface: String)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer): Listener[WS] =
      { locally(actorRefFactory); locally(materializer); ??? }
  def apply(
    http: HttpExt, port: Int, properties: Properties)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer): Listener[WS] =
      { locally(actorRefFactory); locally(materializer); ??? }
  def apply(
    http: HttpExt, port: Int, interface: String,
    properties: Properties)(implicit
    actorRefFactory: ActorSystem,
    materializer: Materializer): Listener[WS] =
      { locally(actorRefFactory); locally(materializer); ??? }
  def apply(port: Int): Listener[WS] = ???
  def apply(port: Int, interface: String): Listener[WS] = ???
  def apply(port: Int, properties: Properties): Listener[WS] = ???
  def apply(port: Int, interface: String,
    properties: Properties): Listener[WS] = ???

  def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(http: HttpExt, url: Uri)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(http: HttpExt, url: String)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(webSocketRequest: WebSocketRequest): Connector[WS] = ???
  def apply(url: Uri): Connector[WS] = ???
  def apply(http: HttpExt, webSocketRequest: WebSocketRequest,
      properties: Properties)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(http: HttpExt, url: Uri, properties: Properties)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(http: HttpExt, url: String, properties: Properties)(
    implicit materializer: Materializer): Connector[WS] =
      { locally(materializer); ??? }
  def apply(webSocketRequest: WebSocketRequest,
    properties: Properties): Connector[WS] = ???
  def apply(url: Uri, properties: Properties): Connector[WS] = ???

  def apply(url: String): Connector[WS] = ???
  def apply(url: String, properties: Properties): Connector[WS] = ???

  trait Secure extends WS with communicator.Secure {
    override def toString = s"WS.Secure($url, $host, $port)"
  }

  object Secure {
    def unapply(ws: Secure) = Some((ws.url, ws.host, ws.port))

    def apply(
      http: HttpExt, port: Int)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS.Secure] =
        { locally(actorRefFactory); locally(materializer); ??? }
    def apply(
      http: HttpExt, port: Int, interface: String)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS.Secure] =
        { locally(actorRefFactory); locally(materializer); ??? }
    def apply(
      http: HttpExt, port: Int, properties: Properties)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS.Secure] =
        { locally(actorRefFactory); locally(materializer); ??? }
    def apply(
      http: HttpExt, port: Int, interface: String,
      properties: Properties)(implicit
      actorRefFactory: ActorSystem,
      materializer: Materializer): Listener[WS.Secure] =
        { locally(actorRefFactory); locally(materializer); ??? }
    def apply(port: Int): Listener[WS.Secure] = ???
    def apply(port: Int, interface: String): Listener[WS.Secure] = ???
    def apply(port: Int, properties: Properties): Listener[WS.Secure] = ???
    def apply(port: Int, interface: String,
      properties: Properties): Listener[WS.Secure] = ???

    def apply(http: HttpExt, webSocketRequest: WebSocketRequest)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(http: HttpExt, url: Uri)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(http: HttpExt, url: String)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(webSocketRequest: WebSocketRequest): Connector[WS.Secure] = ???
    def apply(url: Uri): Connector[WS.Secure] = ???
    def apply(http: HttpExt, webSocketRequest: WebSocketRequest,
        properties: Properties)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(http: HttpExt, url: Uri, properties: Properties)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(http: HttpExt, url: String, properties: Properties)(
      implicit materializer: Materializer): Connector[WS.Secure] =
        { locally(materializer); ??? }
    def apply(webSocketRequest: WebSocketRequest,
      properties: Properties): Connector[WS.Secure] = ???
    def apply(url: Uri, properties: Properties): Connector[WS.Secure] = ???

    def apply(url: String): Connector[WS.Secure] = ???
    def apply(url: String, properties: Properties): Connector[WS.Secure] = ???
  }
}

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
object WebSocketListener {
  def apply(): Listener[WS] with WebSocketRoute = WS.???
  def apply(properties: WS.Properties): Listener[WS] with WebSocketRoute = WS.???

  object Secure {
    def apply(): Listener[WS.Secure] with WebSocketRoute = WS.???
    def apply(properties: WS.Properties): Listener[WS.Secure] with WebSocketRoute = WS.???
  }
}

@compileTimeOnly("Akka WebSocket communicator only available on the JVM")
trait WebSocketRoute extends Route {
  def apply(authenticatedName: String): Route
  def apply(authenticatedName: Option[String]): Route
}
