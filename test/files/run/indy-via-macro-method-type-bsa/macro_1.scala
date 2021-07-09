import java.util.regex._
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros
import java.lang.invoke._

object Macro {
  def methodTypeOf(expr: Any): MethodType = macro Impl.methodTypeOf
}


class Impl(val c: Context) {
  def methodTypeOf(expr: c.Tree): c.Tree = {
    {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val tp = transformedType(expr.asInstanceOf[Tree] match {
        case Block((dd: DefDef) :: Nil, Literal(Constant(()))) =>
          dd.symbol.info
        case expr =>
          expr.tpe
      })
      val bootstrapMethod               = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
      val paramSym                      = NoSymbol.newTermSymbol(TermName("x")).setInfo(typeOf[String])
      val dummySymbol                   = NoSymbol.newTermSymbol(TermName("methodTypeOf")).setInfo(internal.nullaryMethodType(typeOf[java.lang.invoke.MethodType]))
      val bootstrapArgTrees: List[Tree] = List(
        Literal(Constant(bootstrapMethod)).setType(NoType),
        Literal(Constant(tp)).setType(typeOf[java.lang.invoke.MethodType]),
        )
      val result                        = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), bootstrapArgTrees)
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
  }
}
