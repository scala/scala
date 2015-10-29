import java.util.regex._

import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros

object Macro {
  /**
    * Equivalent to Pattern.compile(pat).matcher(text), but caches the compiled regex (using invokedynamic) if
    * `pat` is a literal.
    */
  def matcher(pat: String, text: CharSequence): Matcher = macro Macro.impl
  def impl(c: Context)(pat: c.Tree, text: c.Tree): c.Tree = {
    def Indy(bootstrapMethod: c.Symbol, bootstrapArgs: List[c.universe.Literal], dynArgs: List[c.Tree]): c.Tree = {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val paramSym = NoSymbol.newTermSymbol(TermName("x")).setInfo(typeOf[CharSequence])
      val dummySymbol = NoSymbol.newTermSymbol(TermName("matcher")).setInfo(internal.methodType(paramSym :: Nil, typeOf[java.util.regex.Matcher]))
      val bootstrapArgTrees: List[Tree] = Literal(Constant(bootstrapMethod)).setType(NoType) :: bootstrapArgs.asInstanceOf[List[Tree]]
      val result = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), bootstrapArgTrees ::: dynArgs.asInstanceOf[List[Tree]])
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
    import c.universe._
    pat match {
      case l @ Literal(Constant(pat: String)) =>
        val boostrapSym = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
        Indy(boostrapSym, l :: Nil, text :: Nil)
      case _ =>
        q"_root_.java.util.regex.Pattern.compile($pat).matcher($text)"
    }
  }
}
