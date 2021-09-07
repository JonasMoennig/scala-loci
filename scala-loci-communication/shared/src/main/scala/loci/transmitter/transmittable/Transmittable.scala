package loci
package transmitter
package transmittable

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.concurrent.Future
import scala.quoted.*
import scala.util.Try


final class /[D <: Transmittable.Delegating, T <: Transmittable.Any[?, ?, ?]](
  val tail: D,
  val head: T)
    extends Transmittable.Delegating:
  def tailDelegates = new Transmittables.Delegates(tail)


sealed trait Transmittables extends Any

object Transmittables:
  final class Delegates[T <: Transmittable.Delegating](val delegates: T) extends AnyVal with Transmittables
  final class Message[T <: Transmittable.Any[?, ?, ?]](val message: T) extends AnyVal with Transmittables
  final class None extends Transmittables


sealed trait TransmittableDefaults
  extends TransmittablePrimitives
  with TransmittableCollections

type Transmittable[B, I, R] = Transmittable.Resolution[B, I, R, ?, ? <: Transmittables]

object Transmittable extends TransmittableDefaults:
  sealed trait Delegating

  type Aux[-B, I, +R, P, T <: Transmittables] = Any[B, I, R] {
    type Proxy = P
    type Transmittables = T
  }


  sealed trait Any[-B, I, +R] extends Delegating:
    type Base >: B
    type Intermediate = I
    type Result <: R
    type Proxy
    type Transmittables <: transmittable.Transmittables

    val transmittables: Transmittables

    def buildIntermediate(value: Base)(
      implicit context: Context.Providing[Transmittables]): Intermediate
    def buildResult(value: Intermediate)(
      implicit context: Context.Receiving[Transmittables]): Result
    def buildProxy(value: Notice.Steady[Try[Intermediate]])(
      implicit context: Context.Receiving[Transmittables]): Proxy


  def apply[T](using resolution: Resolution[T, ?, ?, ?, ?]) = resolution.transmittable

  def Argument[T](using resolution: Resolution[T, ?, T, ?, ?]) = resolution.transmittable


  given nothing: IdenticallyTransmittable[Nothing] = IdenticallyTransmittable()


  @implicitNotFound("${B} is not transmittable")
  final class Resolution[B, I, R, P, T <: Transmittables](
      val value: Transmittable.Aux[B, I, R, P, T]) extends AnyVal:
    type Type = Transmittable.Aux[B, I, R, P, T]
    def transmittable: Type = value

  sealed trait ResolutionAlternation:
    given resolutionAlternation[B, I, R](using
      `IsInferred specificity dummy for I`: DummyImplicit,
      `IsInferred specificity dummy for R`: DummyImplicit,
      transmittable: Transmittable.Any[B, I, R])
    : Resolution[B, I, R, transmittable.Proxy, transmittable.Transmittables] =
      Resolution(transmittable)

  sealed trait ResolutionDefault extends ResolutionAlternation:
    given resolution[B, I, R](using
      `IsInferred specificity dummy for I`: DummyImplicit,
      `IsInferred specificity dummy for R`: DummyImplicit,
      transmittable: Transmittable.Any[B, I, R])
    : Resolution[B, I, R, transmittable.Proxy, transmittable.Transmittables] =
      Resolution(transmittable)

  object Resolution extends ResolutionDefault:
    transparent inline given macroGenerated[B, I: IsInferred, R: IsInferred, P, T <: Transmittables](using
      DummyImplicit.Resolvable)
    : Resolution[B, I, R, P, T] =
      ${ TransmittableResolution.optimizedTransmittableResolution[B, I, R, P, T] }


  object Delegating:
    @implicitNotFound("Delegation is not transmittable")
    final class Resolution[D <: Delegating](val transmittables: D) extends AnyVal

    inline given [D <: Delegating]: Resolution[D] =
      ${ TransmittableResolution.delegatingResolution[D] }
end Transmittable


sealed trait IdenticallyTransmittable[B] extends Transmittable.Any[B, B, B] {
  type Base = B
  type Intermediate = B
  type Result = B
  type Proxy = Future[B]
  type Transmittables = Transmittables.None
}

object IdenticallyTransmittable {
  def apply[B](): IdenticallyTransmittable[B] = implementation: Impl[B]

  private sealed trait Impl[-B] extends IdenticallyTransmittable[B @uncheckedVariance]

  private val implementation = new Impl[Any] {
    val transmittables = new Transmittables.None

    def buildIntermediate(value: Base)(
      using context: Context.Providing[Transmittables]) = value

    def buildResult(value: Intermediate)(
      using context: Context.Receiving[Transmittables]) = value

    def buildProxy(value: Notice.Steady[Try[Intermediate]])(
      using context: Context.Receiving[Transmittables]) = value.toFutureFromTry
  }
}


sealed trait TransformingTransmittable[B, I, R] extends Transmittable.Any[B, I, R] {
  type Base = B
  type Intermediate = I
  type Result = R
  type Proxy = Future[R]
  type Transmittables = Transmittables.None
}

object TransformingTransmittable {
  final class Context private[TransformingTransmittable] (val remote: RemoteRef)

  def apply[B, I, R](
      provide: (B, Context) => I,
      receive: (I, Context) => R) =
    new TransformingTransmittable[B, I, R] {
      val transmittables = new Transmittables.None

      def buildIntermediate(value: Base)(
          implicit context: transmittable.Context.Providing[Transmittables]) =
        provide(value, new Context(context.remote))

      def buildResult(value: Intermediate)(
          implicit context: transmittable.Context.Receiving[Transmittables]) =
        receive(value, new Context(context.remote))

      def buildProxy(value: Notice.Steady[Try[Intermediate]])(
          implicit context: transmittable.Context.Receiving[Transmittables]) =
        (value map { _ map buildResult }).toFutureFromTry
    }
}


sealed trait DelegatingTransmittable[B, I, R] extends Transmittable.Any[B, I, R] {
  type Base = B
  type Intermediate = I
  type Result = R
  type Proxy = Future[R]
  type Transmittables = Transmittables.Delegates[Delegates]
  type Delegates <: Transmittable.Delegating
}

object DelegatingTransmittable {
  type Delegates[D <: Transmittable.Delegating] = Transmittables.Delegates[D]

  final class ProvidingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable](
      using context: Context.Providing[Delegates[D]]) {
    val remote = context.remote
    infix def delegate[B, I, R, P, T <: Transmittables](
        value: B)(using selector: Selector.Base[B, I, R, P, T, Delegates[D]]): I =
      context provide value
  }

  final class ReceivingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable](
      using context: Context.Receiving[Delegates[D]]) {
    val remote = context.remote
    infix def delegate[B, I, R, P, T <: Transmittables](
        value: I)(using selector: Selector.Intermediate[B, I, R, P, T, Delegates[D]]): R =
      context receive value
  }

  def apply[B, I, R, D <: Transmittable.Delegating](
      provide: (B, ProvidingContext[D]) => I,
      receive: (I, ReceivingContext[D]) => R)(
    implicit
      delegates: Transmittable.Delegating.Resolution[D]) =
    new DelegatingTransmittable[B, I, R] {
      type Delegates = D

      val transmittables = new Transmittables.Delegates(delegates.transmittables)

      def buildIntermediate(value: Base)(
          implicit context: Context.Providing[Transmittables]) =
        provide(value, new ProvidingContext)

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(value, new ReceivingContext)

      def buildProxy(value: Notice.Steady[Try[Intermediate]])(
          implicit context: Context.Receiving[Transmittables]) =
        (value map { _ map buildResult }).toFutureFromTry
    }
}


sealed trait ConnectedTransmittable[B, I, R] extends Transmittable.Any[B, I, R] {
  type Base = B
  type Intermediate = I
  type Result = R
  type Proxy = Future[R]
  type Transmittables = Transmittables.Message[Message]
  type Message <: Transmittable.Any[?, ?, ?]
}

object ConnectedTransmittable {
  final class Context[B, I, R, P, T <: Transmittables] private[ConnectedTransmittable] (using
      context: transmittable.Context[Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]]) {
    val remote = context.remote
    val endpoint: Endpoint[B, R] = context.endpoint
  }

  def apply[B, R, B0, I0, R0, P0, T0 <: Transmittables](
      provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
      receive: (R0, Context[B0, I0, R0, P0, T0]) => R)(
    implicit
      message: Transmittable.Resolution[B0, I0, R0, P0, T0]) =
    new ConnectedTransmittable[B, I0, R] {
      type Message = Transmittable.Aux[B0, I0, R0, P0, T0]

      val transmittables = new Transmittables.Message(message.transmittable)

      def buildIntermediate(value: Base)(
          implicit context: transmittable.Context.Providing[Transmittables]) =
        context provide provide(value, new Context)

      def buildResult(value: Intermediate)(
          implicit context: transmittable.Context.Receiving[Transmittables]) =
        receive(context receive value, new Context)

      def buildProxy(value: Notice.Steady[Try[Intermediate]])(
          implicit context: transmittable.Context.Receiving[Transmittables]) =
        (value map { _ map buildResult }).toFutureFromTry
    }


  sealed trait Proxy[B, I, R] extends Transmittable.Any[B, I, R] {
    type Base = B
    type Intermediate = I
    type Result = R
    type Transmittables = Transmittables.Message[Message]
    type Message <: Transmittable.Any[?, ?, ?]
    type Internal
  }

  object Proxy {
    def apply[B, R, P, N, B0, I0, R0, P0, T0 <: Transmittables](
        provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
        receive: (R0, Context[B0, I0, R0, P0, T0]) => N,
        direct: (N, Context[B0, I0, R0, P0, T0]) => R,
        proxy: (Notice.Steady[Try[N]], Context[B0, I0, R0, P0, T0]) => P)(
      implicit
        message: Transmittable.Resolution[B0, I0, R0, P0, T0]) =
      new Proxy[B, I0, R] {
        type Message = Transmittable.Aux[B0, I0, R0, P0, T0]
        type Internal = N
        type Proxy = P

        val transmittables = new Transmittables.Message(message.transmittable)

        def buildIntermediate(value: Base)(
            implicit context: transmittable.Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: transmittable.Context.Receiving[Transmittables]) =
          val ctx = new Context[B0, I0, R0, P0, T0]
          direct(receive(context receive value, ctx), ctx)

        def buildProxy(value: Notice.Steady[Try[Intermediate]])(
            implicit context: transmittable.Context.Receiving[Transmittables]) =
          val ctx = new Context[B0, I0, R0, P0, T0]
          proxy(value map { _ map { value => receive(context receive value, ctx) } },  ctx)
      }

    def apply[B, R, P, N, B0, I0, R0, P0, T0 <: Transmittables](
        internal: => N,
        provide: (B, Context[B0, I0, R0, P0, T0]) => B0,
        receive: (N, R0, Context[B0, I0, R0, P0, T0]) => Unit,
        direct: (N, Context[B0, I0, R0, P0, T0]) => R,
        proxy: (N, Notice.Steady[Try[Unit]], Context[B0, I0, R0, P0, T0]) => P)(
      implicit
        message: Transmittable.Resolution[B0, I0, R0, P0, T0]) =
      new Proxy[B, I0, R] {
        type Message = Transmittable.Aux[B0, I0, R0, P0, T0]
        type Internal = N
        type Proxy = P

        val transmittables = new Transmittables.Message(message.transmittable)

        def buildIntermediate(value: Base)(
            implicit context: transmittable.Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: transmittable.Context.Receiving[Transmittables]) = {
          val ctx = new Context[B0, I0, R0, P0, T0]
          val inst = internal
          receive(inst, context receive value, ctx)
          direct(inst, ctx)
        }

        def buildProxy(value: Notice.Steady[Try[Intermediate]])(
            implicit context: transmittable.Context.Receiving[Transmittables]) = {
          val ctx = new Context[B0, I0, R0, P0, T0]
          val inst = internal
          val completion = value map { _ map { _ => () } }
          val result = proxy(inst, completion, ctx)
          value foreach { _ foreach { value => receive(inst, context receive value, ctx) } }
          result
        }
      }
  }
}
