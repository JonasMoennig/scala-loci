package loci

import scala.annotation.compileTimeOnly
import scala.quoted.*

object CompileTimeUtils:
  inline def replace(inline value: String, inline from: String, inline to: String): String =
    ${ replaceImpl('value, 'from, 'to) }

  def replaceImpl(value: Expr[String], from: Expr[String], to: Expr[String])(using Quotes): Expr[String] =
    Expr(value.valueOrError.replace(from.valueOrError, to.valueOrError))

  inline def assertType[T](inline value: Any): Any =
    ${ assertTypeImpl[T]('value) }

  def assertTypeImpl[T: Type](value: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val tpe = value.asTerm.tpe.widenTermRefByName

    if TypeRepr.of[T] =:= tpe then
      '{ () }
    else
      failTest(s"${value.show} has type `${tpe.show}`; type `${TypeRepr.of[T].show}` expected")

  inline def assertExactType[T](inline value: Any): Any =
    ${ assertExactTypeImpl[T]('value) }

  def assertExactTypeImpl[T: Type](value: Expr[Any])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val tpe = value.asTerm.tpe.widenTermRefByName

    if TypeRepr.of[T] == tpe then
      '{ () }
    else
      failTest(s"${value.show} has type of form `${tpe.show}`; exact type `${TypeRepr.of[T].show}` expected")

  inline def containsCompileTimeOnly(inline expr: Any): Boolean =
    ${ containsCompileTimeOnlyImpl('expr) }

  def containsCompileTimeOnlyImpl(expr: Expr[Any])(using Quotes): Expr[Boolean] =
    import quotes.reflect.*

    val compileTimeOnlyAnnotation = TypeRepr.of[compileTimeOnly]

    object compileTimeOnlyAnnotationFinder extends TreeAccumulator[Boolean]:
      def foldTree(compileTimeOnlyFound: Boolean, tree: Tree)(owner: Symbol) =
        compileTimeOnlyFound ||
        (tree.symbol.annotations exists { _.tpe <:< compileTimeOnlyAnnotation }) ||
        foldOverTree(false, tree)(owner)

    Expr(compileTimeOnlyAnnotationFinder.foldTree(false, expr.asTerm)(Symbol.spliceOwner))

  private def failTest(message: String)(using Quotes) =
    import quotes.reflect.*

    '{
      throw new org.scalatest.exceptions.TestFailedException(
        _ => Some(${Expr(message)}),
        None,
        Left(org.scalactic.source.Position.here),
        None,
        Vector.empty)
    }
