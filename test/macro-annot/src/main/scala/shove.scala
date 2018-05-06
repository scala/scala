import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object shoveMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val Apply(Select(New(AppliedTypeTree(_, List(victim))), _), _) = c.prefix.tree
    val result = {
      annottees.map(_.tree).toList match {
        case ValDef(mods, name, tpt, rhs) :: Nil =>
          ClassDef(
            Modifiers(IMPLICIT),
            TypeName(s"${victim}With$name"),
            List(),
            Template(
              List(Select(Ident(TermName("scala")), TypeName("AnyRef"))),
              noSelfType,
              List(
                ValDef(Modifiers(PRIVATE | LOCAL), TermName("x"), victim, EmptyTree),
                DefDef(
                  Modifiers(),
                  termNames.CONSTRUCTOR,
                  List(),
                  List(List(ValDef(Modifiers(PARAM), TermName("x"), victim, EmptyTree))),
                  TypeTree(),
                  Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))
                ),
                ValDef(mods, name, tpt, rhs)
              )
            )
          )
      }
    }
    c.Expr[Any](result)
  }
}

class shove[A] extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro shoveMacro.impl
}

package pkg {
  class shove extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro shoveMacro.impl
  }
}