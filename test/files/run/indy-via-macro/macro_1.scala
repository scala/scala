import java.util.regex.Pattern

import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros

object Macro {
  /**
    * Equivalent to Pattern.compile(s), but caches the compiled regex (using invokedynamic) if
    * `s` is a literal.
    */
  def compilePattern(s: String): Pattern = macro Macro.impl
  def impl(c: Context)(s: c.Tree): c.Tree = {
    def Indy(bootstrapMethod: c.Symbol, bootstrapArgs: List[c.universe.Literal]): c.Tree = {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val dummySymbol = NoSymbol.newTermSymbol(TermName("compile")).setInfo(NullaryMethodType(typeOf[Pattern]))
      val args: List[Tree] = Literal(Constant(bootstrapMethod)).setType(NoType) :: bootstrapArgs.asInstanceOf[List[Tree]]
      val result = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), args)
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
    import c.universe._
    s match {
      case l @ Literal(Constant(s: String)) =>
        val boostrapSym = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
        Indy(boostrapSym, l :: Nil)
      case _ =>
        q"_root_.java.util.regex.Pattern.compile($s)"
    }
  }
}
