package bakery

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

trait FailureCake {
  implicit def liftAnyFails[T: Manifest]: Any = ???

  // This works
  // implicit def liftAny[T]: Any = ???
}

object Bakery {

  def failure: Any = macro failureImpl
  def failureImpl(c: Context): c.Expr[Any] = {
    import c.universe._

    def dslTrait(dslName: String) = {
      val names = dslName.split("\\.").toList.reverse
      assert(names.length >= 1, "DSL trait name must be in the valid format. DSL trait name is " + dslName)

      val tpeName = newTypeName(names.head)
      names.tail.reverse match {
        case head :: tail ⇒
          Select(tail.foldLeft[Tree](Ident(newTermName(head)))((tree, name) ⇒ Select(tree, newTermName(name))), tpeName)
        case Nil ⇒
          Ident(tpeName)
      }
    }

    def composeDSL(transformedBody: Tree) =
      ClassDef(Modifiers(), newTypeName("eval"), List(), Template(
        List(dslTrait("bakery.FailureCake")),
        emptyValDef,
        List(
          DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(),
            Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))),
          DefDef(Modifiers(), newTermName("main"), List(), List(List()), Ident(newTypeName("Any")), transformedBody))))

    def constructor = Apply(Select(New(Ident(newTypeName("eval"))), termNames.CONSTRUCTOR), List())

    c.eval(c.Expr[Any](
      c.untypecheck(Block(composeDSL(Literal(Constant(1))), constructor))))

    c.Expr[Any](Literal(Constant(1)))
  }
}