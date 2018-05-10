import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object funnyMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val (annottee: MemberDef) :: (expandee: MemberDef) :: rest = annottees.map(_.tree).toList
    def funnify[T <: Name](name: T): T = {
      val sfunnified = name.toString + annottee.name.toString
      val funnified = if (name.isTermName) TermName(sfunnified) else TypeName(sfunnified)
      funnified.asInstanceOf[T]
    }
    val expandee1 = {
      expandee match {
        case ClassDef(mods, name, tparams, impl) => ClassDef(mods, funnify(name), tparams, impl)
        case ModuleDef(mods, name, impl) => ModuleDef(mods, funnify(name), impl)
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) => DefDef(mods, funnify(name), tparams, vparamss, tpt, rhs)
        case TypeDef(mods, name, tparams, rhs) => TypeDef(mods, funnify(name), tparams, rhs)
        case ValDef(mods, name, tpt, rhs) => ValDef(mods, funnify(name), tpt, rhs)
      }
    }
    c.Expr[Any](Block(expandee1 :: rest, Literal(Constant(()))))
  }
}

class funny extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro funnyMacro.impl
}

package pkg {
  class funny extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro funnyMacro.impl
  }
}