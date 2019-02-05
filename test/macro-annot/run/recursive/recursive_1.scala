import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation

object socialMacros {
  def plusOne(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    import Flag._
    val result = {
      annottees.map(_.tree).toList match {
        case ClassDef(mods, name, tparams, Template(parents, self, body)) :: rest =>
          val beforeToString = body.collect{ case ddef @ DefDef(_, name, _, _, _, _) if name == TermName("toString") => ddef }
          val List(DefDef(_, _, _, _, _, Literal(Constant(before: String)))) = beforeToString
          val after = before + "+1"
          val afterToString = DefDef(Modifiers(OVERRIDE), TermName("toString"), List(), List(List()), TypeTree(), Literal(Constant(after)))
          val body1 = body.diff(beforeToString) :+ afterToString
          ClassDef(mods, name, tparams, Template(parents, self, body1)) :: rest
      }
    }
    c.Expr[Any](Block(result, Literal(Constant(()))))
  }
  def plusTwo(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._
    val result = {
      annottees.map(_.tree).toList match {
        case ClassDef(mods, name, tparams, impl) :: rest =>
          def plusOne = Apply(Select(New(Ident(TypeName("plusOne"))), termNames.CONSTRUCTOR), Nil)
          val mods1 = Modifiers(mods.flags, mods.privateWithin, mods.annotations :+ plusOne :+ plusOne)
          ClassDef(mods1, name, tparams, impl) :: rest
      }
    }
    c.Expr[Any](Block(result, Literal(Constant(()))))
  }
}

class plusOne extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro socialMacros.plusOne
}

class plusTwo extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro socialMacros.plusTwo
}

package pkg {
  class plusOne extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro socialMacros.plusOne
  }

  class plusTwo extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro socialMacros.plusTwo
  }
}
