import java.util.regex._
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros
import java.lang.invoke._

object Macro {
  def classNameOf(expr: Class[_]): String = macro Impl.classNameOf
}


class Impl(val c: Context) {
  def classNameOf(expr: c.Tree): c.Tree = {
    {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val bootstrapMethod               = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
      val paramSym                      = NoSymbol.newTermSymbol(TermName("x")).setInfo(typeOf[String])
      val dummySymbol                   = NoSymbol.newTermSymbol(TermName("classNameOf")).setInfo(internal.nullaryMethodType(typeOf[String]))
      val bootstrapArgTrees: List[Tree] = List(
        Literal(Constant(bootstrapMethod)).setType(NoType),
        expr.asInstanceOf[Tree],
        )
      val result                        = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), bootstrapArgTrees)
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
  }
}
