package loci
package transmitter
package transmittable

import Selector.*

import scala.compiletime.ops.int
import scala.quoted.*
import scala.runtime.Tuples

object GenericTuple:
  sealed trait IdenticalDelegation[T <: Tuple]

  object IdenticalDelegation:
    given empty: IdenticalDelegation[EmptyTuple]()
    given nonempty[H: IdenticallyTransmittable, T <: Tuple: IdenticalDelegation]: IdenticalDelegation[H *: T]()


  final class Delegation[B <: NonEmptyTuple, I <: NonEmptyTuple, R <: NonEmptyTuple, D <: Transmittable.Delegating, Outer <: Boolean](
      val delegating: D,
      val provide: (B, DelegatingTransmittable.ProvidingContext[D]) => I,
      val receive: (I, DelegatingTransmittable.ReceivingContext[D]) => R):
    type Delegating = D

  sealed trait MultipleElementsDelegation:
    transparent inline given multiple[
        B, I, R, P, T <: Transmittables,
        BT <: NonEmptyTuple, IT <: NonEmptyTuple, RT <: NonEmptyTuple,
        D <: Transmittable.Delegating, Outer <: Boolean](using
      inline resolution: Transmittable.Resolution[B, I, R, P, T],
      inline delegation: Delegation[BT, IT, RT, D, false])
    : Delegation[B *: BT, I *: IT, R *: RT, D / Transmittable.Aux[B, I, R, P, T], Outer] =
      ${ Delegation.multipleImpl[B, I, R, P, T, BT, IT, RT, D, Outer]('resolution, 'delegation) }

    def multipleImpl[
        B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type,
        BT <: NonEmptyTuple: Type, IT <: NonEmptyTuple: Type, RT <: NonEmptyTuple: Type,
        D <: Transmittable.Delegating: Type, Outer <: Boolean: Type](
      resolution: Expr[Transmittable.Resolution[B, I, R, P, T]],
      delegation: Expr[Delegation[BT, IT, RT, D, false]])(using Quotes)
    : Expr[Delegation[B *: BT, I *: IT, R *: RT, D / Transmittable.Aux[B, I, R, P, T], Outer]] =
      import quotes.reflect.*

      val transmittable = resolution match
        case '{ Transmittable.Resolution[B, I, R, P, T]($transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolution[B, I, R](using $dummyI, $dummyB, $transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolutionAlternation[B, I, R](using $dummyI, $dummyB, $transmittable) } => transmittable

      val '{ new Delegation[BT, IT, RT, D, false]($delegates, $provide, $receive) } = delegation

      val (provideTransformation, receiveTransformation) =
        makeTransformations[B *: BT, I *: IT, R *: RT, D / Transmittable.Aux[B, I, R, P, T], Outer]

      '{ Delegation(
          /($delegates, ${transmittable.asExprOf[Transmittable.Aux[B, I, R, P, T]]}),
          $provideTransformation,
          $receiveTransformation) }
    end multipleImpl
  end MultipleElementsDelegation

  object Delegation extends MultipleElementsDelegation:
    transparent inline given single[B, I, R, P, T <: Transmittables, Outer <: Boolean](using
      inline resolution: Transmittable.Resolution[B, I, R, P, T])
    : Delegation[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Transmittable.Aux[B, I, R, P, T], Outer] =
      ${ Delegation.singleImpl[B, I, R, P, T, Outer]('resolution) }

    def singleImpl[B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type, Outer <: Boolean: Type](
      resolution: Expr[Transmittable.Resolution[B, I, R, P, T]])(using Quotes)
    : Expr[Delegation[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Transmittable.Aux[B, I, R, P, T], Outer]] =
      import quotes.reflect.*

      val transmittable = resolution match
        case '{ Transmittable.Resolution[B, I, R, P, T]($transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolution[B, I, R](using $dummyI, $dummyB, $transmittable) } => transmittable
        case '{ Transmittable.Resolution.resolutionAlternation[B, I, R](using $dummyI, $dummyB, $transmittable) } => transmittable

      val (provideTransformation, receiveTransformation) =
        makeTransformations[B *: EmptyTuple, I *: EmptyTuple, R *: EmptyTuple, Transmittable.Aux[B, I, R, P, T], Outer]

      '{ Delegation(
          ${transmittable.asExprOf[Transmittable.Aux[B, I, R, P, T]]},
          $provideTransformation,
          $receiveTransformation) }
    end singleImpl
  end Delegation


  private def makeTransformations[
      B <: Tuple: Type, I <: Tuple: Type, R <: Tuple: Type,
      D <: Transmittable.Delegating: Type, Outer <: Boolean: Type](using Quotes) =
    import quotes.reflect.*

    val Block(List(typeDefinition), _) = '{ type Delegating = D }.asTerm.underlying

    if Type.valueOfConstant[Outer] contains true then
      given Type[D] = TypeIdent(typeDefinition.symbol).tpe.asType.asInstanceOf[Type[D]]

      val (baseSelectors, intermediateSelectors) = makeSelectors[B, I, R, D]

      val provideTransformation = '{
        import scala.language.unsafeNulls
        type Delegating = D
        (value: B, context: DelegatingTransmittable.ProvidingContext[D]) =>
          (if value == null then
             null
           else
             Tuples.fromIArray((value.productIterator zip ${Expr.ofSeq(baseSelectors)}.iterator map { valueSelector =>
               (context delegate valueSelector._1.asInstanceOf[Object])(using valueSelector._2)
             }).toArray.asInstanceOf[IArray[Object]])).asInstanceOf[I]
      }

      val receiveTransformation = '{
        import scala.language.unsafeNulls
        type Delegating = D
        (value: I, context: DelegatingTransmittable.ReceivingContext[D]) =>
          (if value == null then
             null
           else
             Tuples.fromIArray((value.productIterator zip ${Expr.ofSeq(intermediateSelectors)}.iterator map { valueSelector =>
               (context delegate valueSelector._1.asInstanceOf[Object])(using valueSelector._2)
             }).toArray.asInstanceOf[IArray[Object]])).asInstanceOf[R]
      }

      object typeDefinitionReplacer extends TreeMap:
        override def transformStatement(tree: Statement)(owner: Symbol) = tree match
          case TypeDef(_, _) => typeDefinition
          case _ => super.transformStatement(tree)(owner)

        def apply(tree: Tree) =
          transformTree(tree: Tree)(Symbol.spliceOwner)

      typeDefinitionReplacer(provideTransformation.asTerm).asExprOf[(B, DelegatingTransmittable.ProvidingContext[D]) => I] ->
      typeDefinitionReplacer(receiveTransformation.asTerm).asExprOf[(I, DelegatingTransmittable.ReceivingContext[D]) => R]
    else
      '{ ??? } -> '{ ??? }
  end makeTransformations

  private def makeSelectors[B <: Tuple: Type, I <: Tuple: Type, R <: Tuple: Type, D <: Transmittable.Delegating: Type](using Quotes) =
    import Transmittables.Delegates
    import quotes.reflect.*

    def makeSelectors[B <: Tuple: Type, I <: Tuple: Type, R <: Tuple: Type, T <: Transmittable.Delegating: Type, N <: Int: Type]
        : List[(
            Expr[Base[Object, Object, Object, Object, Transmittables, Delegates[D]]],
            Expr[Intermediate[Object, Object, Object, Object, Transmittables, Delegates[D]]])] =
      val n = TypeRepr.of[N]

      def makeSelector[B: Type, I: Type, R: Type, P: Type, T <: Transmittables: Type] =
        import scala.language.unsafeNulls

        given Type[N] = n.simplified.asType.asInstanceOf[Type[N]]
        val value = Expr(Type.valueOfConstant[N].get)

        '{ Base(using null, ValueOf($value)).asInstanceOf[Base[Object, Object, Object, Object, Transmittables, Delegates[D]]] } ->
        '{ Intermediate(using null, ValueOf($value)).asInstanceOf[Intermediate[Object, Object, Object, Object, Transmittables, Delegates[D]]] }

      (Type.of[B], Type.of[I], Type.of[R], Type.of[T]) match
        case ('[ tb *: tbt ], '[ ti *: tit ], '[ tr *: trt ], '[ d / Transmittable.Aux[b, i, r, p, t] ])
            if TypeRepr.of[tb] =:= TypeRepr.of[b] && TypeRepr.of[ti] =:= TypeRepr.of[i] && TypeRepr.of[tr] =:= TypeRepr.of[r] =>
          makeSelector[b, i, r, p, t] :: makeSelectors[tbt, tit, trt, d, int.S[N]]
        case ('[ tb *: EmptyTuple ], '[ ti *: EmptyTuple ], '[ tr *: EmptyTuple ], '[ Transmittable.Aux[b, i, r, p, t] ])
            if TypeRepr.of[tb] =:= TypeRepr.of[b] && TypeRepr.of[ti] =:= TypeRepr.of[i] && TypeRepr.of[tr] =:= TypeRepr.of[r] =>
          List(makeSelector[b, i, r, p, t])

    makeSelectors[B, I, R, D, 0].unzip
  end makeSelectors
end GenericTuple
