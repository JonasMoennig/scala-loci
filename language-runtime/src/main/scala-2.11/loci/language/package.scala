package loci

import communicator._
import embedding._
import messaging._

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros

package language {
  final class peer extends StaticAnnotation

  sealed trait Single[P] extends Multiple[P]
  sealed trait Optional[P] extends Multiple[P]
  sealed trait Multiple[+P]

  trait Remote[+P] extends Equals
  object Remote extends transmitter.RemoteReference
}

package object language {
  type Local[T] = T

  type on[T, P] = Placed[T, P] with T
  type per[T, P] = Placed.Subjective[T, P]

  def connect[P](setup: Connector[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def connect[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory

  def listen[P](setup: Listener[ConnectionsBase.Protocol]): Connections =
    macro impl.Connections.setup
  def listen[P](factory: ConnectionSetupFactory[ConnectionsBase.Protocol])(
      /* url: String, props: ConnectionSetupFactory.Properties */ args: Any*): Connections =
    macro impl.Connections.factory

  def placed: Placement.Placed = erased
  def on: Placement.Select[Placement.Run] = erased
  def on[P]: Placement.On[P] with Placement.Run[P, from] = erased
  def remote: Placement.Narrow with Placement.Select[Placement.Call] with Placement.Call[Nothing, from] with Gateway[Nothing] = erased
  def remote[P]: Placement.Call[P, from] with Gateway[P] = erased
}
