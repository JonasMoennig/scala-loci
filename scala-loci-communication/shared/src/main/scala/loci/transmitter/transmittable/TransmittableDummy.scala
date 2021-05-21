package loci
package transmitter
package transmittable

import scala.annotation.compileTimeOnly
import scala.concurrent.Future
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait TransmittableDummy {
  this: TransmittableBase.type =>

  @compileTimeOnly("Value is not transmittable")
  final implicit def resolutionFailure[
      B, I, R, P, T <: Transmittables,
      TransmittableFallback[B, I, R, P, T <: Transmittables]]: TransmittableFallback[B, I, R, P, T]
    = macro TransmittableResolutionFailure[B, I, R, P, T]

  @compileTimeOnly("Value is not transmittable")
  final def dummy[B, I, R, P, T <: Transmittables]: Transmittable.Aux[B, I, R, P, T]
    = throw new NotImplementedError
}

object TransmittableResolutionFailure {
  def apply[
      B: c.WeakTypeTag,
      I: c.WeakTypeTag,
      R: c.WeakTypeTag,
      P: c.WeakTypeTag,
      T <: Transmittables: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val B = weakTypeOf[B]
    val I = weakTypeOf[I]
    val R = weakTypeOf[R]
    val P = weakTypeOf[P]
    val T = weakTypeOf[T]

    val none = typeOf[Transmittables.None]
    val transmittableDummy = symbolOf[TransmittableDummy]
    val TypeRef(futurePre, futureSym, _) = typeOf[Future[Any]]: @unchecked
    val ExistentialType(existentialQuantified, TypeRef(pre, sym, existentialArgs)) =
      typeOf[Transmittable.Any[_, _, _]]: @unchecked

    def originalType(tpe: Type) = tpe match {
      case TypeRef(_, _, List(_, original, ConstantType(Constant(name: String))))
          if tpe <:< typeOf[TransmittableBase.SurrogateType[_, _, _]] =>
        original -> name
      case tpe =>
        tpe -> tpe.toString
    }

    def instantiatedTypeOrElse(tpe: Type, alternative: Type, alternativeSymbol: Symbol) = {
      val symbol = tpe.typeSymbol
      if (symbol.owner.owner == transmittableDummy)
        alternative -> List(alternativeSymbol)
      else
        tpe -> List.empty
    }

    def instantiatedOriginalTypeOrElse(tpe: Type, alternative: Type, alternativeSymbol: Symbol) = {
      val (original, _) = originalType(tpe)
      instantiatedTypeOrElse(original, alternative, alternativeSymbol)
    }

    val (typeI, _) = instantiatedTypeOrElse(I, B, NoSymbol)
    val (typeR, _) = instantiatedTypeOrElse(R, B, NoSymbol)
    val (typeP, _) = instantiatedTypeOrElse(P, internal.typeRef(futurePre, futureSym, List(B)), NoSymbol)
    val (typeT, _) = instantiatedTypeOrElse(T, none, NoSymbol)

    val (originalB, nameB) = originalType(B)
    val symbolB = originalB.typeSymbol

    val (args, quantified) =
      (List(B, I, R, P, T) zip existentialQuantified zip existentialArgs).foldRight(List.empty[(Type, List[Symbol])]) {
        case (((tpe, symbol), arg), args) =>
          instantiatedOriginalTypeOrElse(tpe, arg, symbol) :: args
      }.unzip

    val transmittableTypeRef = internal.typeRef(pre, sym, args)

    val transmittableType =
      if (quantified.nonEmpty)
        internal.existentialType(quantified.flatten, transmittableTypeRef)
      else
        transmittableTypeRef

    val baseMessage = s"$nameB is not transmittable"

    val hintMessage =
      if (symbolB.isClass && symbolB.asClass.isCaseClass) {
        val impl = if (symbolB.isModuleClass) "case object" else "case class"
        s"$baseMessage; you may consider defining an `IdenticallyTransmittable[$originalB]` instance for $impl ${symbolB.name}"
      }
      else
        baseMessage

    val message = s"$hintMessage${utility.implicitHints.values(c)(transmittableType)}"

    q"""{
      @${termNames.ROOTPKG}.scala.annotation.compileTimeOnly($message) def resolutionFailure() = ()
      resolutionFailure()
      ${termNames.ROOTPKG}.loci.transmitter.transmittable.TransmittableBase.dummy[$B, $typeI, $typeR, $typeP, $typeT]
    }"""
  }
}
