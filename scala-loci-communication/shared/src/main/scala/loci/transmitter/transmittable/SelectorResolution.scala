package loci
package transmitter
package transmittable

import scala.quoted.*

object SelectorResolution:
  final class Index[M <: AnyKind, D <: AnyKind, / <: AnyKind, T, V, S, N <: Int]

  object Index:
    transparent inline given [M <: AnyKind, D <: AnyKind, / <: AnyKind, T, V, S, N <: Int](
        using IndexComputation[M, D, /, T, V, S, N])
      : Index[M, D, /, T, V, S, N] = ${ indexImpl[M, D, /, T, V, S, N] }


  final class IndexComputation[M <: AnyKind, D <: AnyKind, / <: AnyKind, T, V, S, -N]

  object IndexComputation:
    transparent inline given [M <: AnyKind, D <: AnyKind, / <: AnyKind, T, V, S]
      : IndexComputation[M, D, /, T, V, S, ?] = ${ indexComputationImpl[M, D, /, T, V, S] }


  def indexImpl[M <: AnyKind: Type, D <: AnyKind: Type, / <: AnyKind: Type, T: Type, V: Type, S: Type, N <: Int: Type](using Quotes) =
    import quotes.reflect.*
    '{ Index[M, D, /, T, V, S, N] }

  def indexComputationImpl[M <: AnyKind: Type, D <: AnyKind: Type, / <: AnyKind: Type, T: Type, V: Type, S: Type](using Quotes) =
    import quotes.reflect.*

    val message = TypeRepr.of[M].typeSymbol
    val delegates = TypeRepr.of[D].typeSymbol
    val list = TypeRepr.of[/].typeSymbol

    val transmittableMember = TypeRepr.of[T].typeSymbol
    val transmittableClass = transmittableMember.owner

    val V = TypeRepr.of[V]
    val S = TypeRepr.of[S]

    val (checkLow, checkHigh) =
      transmittableMember.tree match
        case TypeDef(_, tree: TypeTree) => tree.tpe match
          case TypeBounds(low, hi) =>
            (low.typeSymbol != defn.NothingClass || !hi.typeSymbol.flags.is(Flags.Covariant)) ->
            (hi.typeSymbol != defn.AnyClass || !low.typeSymbol.flags.is(Flags.Contravariant))
          case _ => true -> true
        case _ => true -> true

    def matchesMember(transmittable: TypeRepr) =
      if transmittable.derivesFrom(transmittableClass) then
        transmittable.memberType(transmittableMember) match
          case TypeBounds(low, hi) =>
            (!checkLow || low =:= V) && (!checkHigh || hi =:= V)
          case tpe =>
            tpe =:= V
      else
        false

    def fail =
      val transmittableOwner = transmittableClass.owner
      val transmittableCompanion = transmittableOwner.companionModule match
        case companion if companion != Symbol.noSymbol => companion
        case _ => transmittableOwner

      report.throwError(
        s"${transmittableCompanion.name}.${transmittableClass.name} " +
        s"with $transmittableMember = ${V.show} not specified in: ${S.dealias.show}")

    def result(n: Int) =
      ConstantType(IntConstant(n)).asType match
        case '[n] =>
          '{ IndexComputation[M, D, /, T, V, S, n] }

    def index(delegating: TypeRepr)(n: Int): Expr[IndexComputation[M, D, /, T, V, S, Nothing]] =
      delegating.baseType(list) match
        case AppliedType(_, List(tail, head)) =>
          if matchesMember(head) then result(n)
          else index(tail)(n + 1)
        case _ if matchesMember(delegating) => result(n)
        case _ => fail

    S.baseType(message) match
      case AppliedType(_, List(transmittable)) =>
        if matchesMember(transmittable) then result(0)
        else fail
      case _ =>
        S.baseType(delegates) match
          case AppliedType(_, List(delegating)) => index(delegating)(0)
          case _ => fail

end SelectorResolution
