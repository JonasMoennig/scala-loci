package loci
package transmitter

import scala.annotation.unchecked.uncheckedVariance
import scala.annotation.{compileTimeOnly, implicitNotFound}
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.util.Try


final class /[D <: Transmittable.Delegating, T <: Transmittable.Any[_, _, _]](
    val tail: D, val head: T) extends Transmittable.Delegating {
  def tailDelegates = new Transmittables.Delegates(tail)
}


sealed trait Transmittables extends Any

object Transmittables {
  final class Delegates[T <: Transmittable.Delegating](val delegates: T)
    extends AnyVal with Transmittables

  final class Message[T <: Transmittable.Any[_, _, _]](val message: T)
    extends AnyVal with Transmittables

  final class None extends Transmittables
}


object TransmittableBase extends
    TransmittablePrimitives with
    TransmittableTuples with
    TransmittableCollections {

  sealed trait Delegating

  sealed trait Any[-B, I, +R] extends Delegating {
    type Base >: B
    type Intermediate = I
    type Result <: R
    type Proxy
    type Transmittables <: transmitter.Transmittables

    val transmittables: Transmittables

    def buildIntermediate(value: Base)(
      implicit context: Context.Providing[Transmittables]): Intermediate

    def buildResult(value: Intermediate)(
      implicit context: Context.Receiving[Transmittables]): Result

    def buildProxy(value: Notice.Steady[Try[Intermediate]])(
      implicit context: Context.Receiving[Transmittables]): Proxy
  }


  @implicitNotFound("${B} is not transmittable")
  final class Wrapper[B, I, R, P, T <: Transmittables](
      val transmittable: Transmittable.Aux[B, I, R, P, T]) extends AnyVal {
    type Base = B
    type Intermediate = I
    type Result = R
    type Proxy = P
    type Transmittables = T
    type Type = Transmittable.Aux[B, I, R, P, T]
  }

  sealed trait WrapperAlternation {
    implicit def wrapperAlternation[B, I, R, P, T <: Transmittables](implicit
      transmittable: Transmittable.Aux[B, I, R, P, T])
    : Wrapper[B, I, R, P, T] =
      new Wrapper(transmittable)
  }

  object Wrapper extends WrapperAlternation {
    implicit def wrapper[B, I, R, P, T <: Transmittables](implicit
      transmittable: Transmittable.Aux[B, I, R, P, T])
    : Wrapper[B, I, R, P, T] =
      new Wrapper(transmittable)
  }


  implicit def nothing: IdenticallyTransmittable[Nothing] =
    IdenticallyTransmittable()


  sealed trait SurrogateType[T, U, V]

  object SurrogateType {
    @compileTimeOnly("loci.transmitter.transmittable.TransmittableBase.SurrogateType is not transmittable")
    implicit def surrogateType[T, V]: IdenticallyTransmittable[SurrogateType[T, Nothing, V]] =
      IdenticallyTransmittable()
  }


  @implicitNotFound("${B} is not transmittable")
  final class DependantValue[B, I, R, +V] private (val value: V) extends AnyVal

  object DependantValue {
    implicit def dependantValue[B, I, R, P, T <: Transmittables](implicit
      wrapper: Wrapper[B, I, R, P, T])
    : DependantValue[B, I, R, wrapper.Type] =
      new DependantValue(wrapper.transmittable)
  }


  @implicitNotFound("${B} is not transmittable")
  final class Resolution[B, I, R, P, T <: Transmittables](
      val value: Transmittable.Aux[B, I, R, P, T]) extends AnyVal {
    type Type = Transmittable.Aux[B, I, R, P, T]
    def transmittable: Type = value
  }

  sealed trait ResolutionFailure {
    @compileTimeOnly("Value is not transmittable")
    implicit def resolutionFailure[B, I, R, P, T <: Transmittables](implicit
      dummy: DummyImplicit.Unresolvable)
    : Resolution[B, I, R, P, T] = {
      locally(dummy)
      throw new NotImplementedError
    }
  }

  sealed trait ResolutionDefault extends ResolutionFailure {
    implicit def default[B, I, R, P, T <: Transmittables](implicit
      dependant: DependantValue[B, I, R, Transmittable.Aux[B, I, R, P, T]])
    : Resolution[B, I, R, P, T] =
      new Resolution(dependant.value)
  }

  sealed trait ResolutionNothing extends ResolutionDefault {
    implicit def nothing(implicit
      dummy: DummyImplicit.Unresolvable)
    : Resolution[Nothing, Nothing, Nothing, Future[Nothing], Transmittables.None] ={
      locally(dummy)
      throw new NotImplementedError
    }
  }

  object Resolution extends ResolutionNothing {
    implicit def macroGenerated[B, I, R, P, T <: Transmittables](implicit
      dummy: DummyImplicit.Resolvable)
    : Resolution[B, I, R, P, T] =
      macro TransmittableResolution[B, I, R, P, T]
  }


  sealed trait DelegatingFailure {
    @compileTimeOnly("Delegation is not transmittable")
    implicit def resolutionFailure[D <: Delegating](implicit
      dummy: DummyImplicit.Unresolvable)
    : Delegating.Resolution[D] = {
      locally(dummy)
      throw new NotImplementedError
    }
  }

  object Delegating extends DelegatingFailure {
    final class Resolution[D <: Delegating](
      val transmittables: D) extends AnyVal

    implicit def single[B, I, R, P, T <: Transmittables](implicit
      resolution: Transmittable.Resolution[B, I, R, P, T])
    : Resolution[Transmittable.Aux[B, I, R, P, T]] =
      new Resolution(resolution.transmittable)

    implicit def list[B, I, R, P, T <: Transmittables, D <: Delegating](implicit
      resolution: Transmittable.Resolution[B, I, R, P, T],
      delegates: Resolution[D])
    : Resolution[D / Transmittable.Aux[B, I, R, P, T]] =
      new Resolution(new / (delegates.transmittables, resolution.transmittable))
  }
}


sealed trait IdenticallyTransmittable[B] extends Transmittable.Any[B, B, B] {
  override type Base = B
  override type Intermediate = B
  override type Result = B
  type Proxy = Future[B]
  type Transmittables = Transmittables.None
}

object IdenticallyTransmittable {
  def apply[B](): IdenticallyTransmittable[B] = implementation: Impl[B]

  private sealed trait Impl[-B] extends IdenticallyTransmittable[B @uncheckedVariance]

  private val implementation = new Impl[Any] {
    val transmittables = new Transmittables.None

    def buildIntermediate(value: Base)(
      implicit context: Context.Providing[Transmittables]) = value

    def buildResult(value: Intermediate)(
      implicit context: Context.Receiving[Transmittables]) = value

    def buildProxy(value: Notice.Steady[Try[Intermediate]])(
      implicit context: Context.Receiving[Transmittables]) = value.toFutureFromTry
  }
}


sealed trait TransformingTransmittable[B, I, R] extends Transmittable.Any[B, I, R] {
  override type Base = B
  override type Intermediate = I
  override type Result = R
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
          implicit context: Context.Providing[Transmittables]) =
        provide(value, new Context(context.remote))

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(value, new Context(context.remote))

      def buildProxy(value: Notice.Steady[Try[Intermediate]])(
          implicit context: Context.Receiving[Transmittables]) =
        (value map { _ map buildResult }).toFutureFromTry
    }
}


sealed trait DelegatingTransmittable[B, I, R] extends Transmittable.Any[B, I, R] {
  override type Base = B
  override type Intermediate = I
  override type Result = R
  type Proxy = Future[R]
  type Transmittables = Transmittables.Delegates[Delegates]
  type Delegates <: Transmittable.Delegating
}

object DelegatingTransmittable {
  type Delegates[D <: Transmittable.Delegating] = Transmittables.Delegates[D]

  final class ProvidingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable](
      implicit context: Context.Providing[Delegates[D]]) {
    val remote = context.remote
    def delegate[B, I, R, P, T <: Transmittables](
        value: B)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): I =
      context provide value
  }

  final class ReceivingContext[D <: Transmittable.Delegating] private[DelegatingTransmittable](
      implicit context: Context.Receiving[Delegates[D]]) {
    val remote = context.remote
    def delegate[B, I, R, P, T <: Transmittables](
        value: I)(implicit selector: Selector[B, I, R, P, T, Delegates[D]]): R =
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
  override type Base = B
  override type Intermediate = I
  override type Result = R
  type Proxy = Future[R]
  type Transmittables = Transmittables.Message[Message]
  type Message <: Transmittable.Any[_, _, _]
}

object ConnectedTransmittable {
  final class Context[B, I, R, P, T <: Transmittables] private[ConnectedTransmittable] (implicit
      context: transmitter.Context[Transmittables.Message[Transmittable.Aux[B, I, R, P, T]]]) {
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
          implicit context: Context.Providing[Transmittables]) =
        context provide provide(value, new Context)

      def buildResult(value: Intermediate)(
          implicit context: Context.Receiving[Transmittables]) =
        receive(context receive value, new Context)

      def buildProxy(value: Notice.Steady[Try[Intermediate]])(
          implicit context: Context.Receiving[Transmittables]) =
        (value map { _ map buildResult }).toFutureFromTry
    }


  sealed trait Proxy[B, I, R] extends Transmittable.Any[B, I, R] {
    override type Base = B
    override type Intermediate = I
    override type Result = R
    type Transmittables = Transmittables.Message[Message]
    type Message <: Transmittable.Any[_, _, _]
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
            implicit context: Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          direct(receive(context receive value, ctx), ctx)
        }

        def buildProxy(value: Notice.Steady[Try[Intermediate]])(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          proxy(value map { _ map { value => receive(context receive value, ctx) } },  ctx)
        }
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
            implicit context: Context.Providing[Transmittables]) =
          context provide provide(value, new Context)

        def buildResult(value: Intermediate)(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          val inst = internal
          receive(inst, context receive value, ctx)
          direct(inst, ctx)
        }

        def buildProxy(value: Notice.Steady[Try[Intermediate]])(
            implicit context: Context.Receiving[Transmittables]) = {
          val ctx = new Context
          val inst = internal
          val completion = value map { _ map { _ => () } }
          val result = proxy(inst, completion, ctx)
          value foreach { _ foreach { value => receive(inst, context receive value, ctx) } }
          result
        }
      }
  }
}
