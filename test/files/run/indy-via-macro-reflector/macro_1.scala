import java.util.regex._
import scala.reflect.internal.SymbolTable
import scala.reflect.macros.blackbox._
import language.experimental.macros
import java.lang.invoke.{MethodHandle, MethodHandles}

object Macro {
  def reflector(dynamic: String): String = macro Impl.reflector
  def reflectorConstructor(dynamic: String): String = macro Impl.reflectorConstructor
  def reflectorTrait(dynamic: String): String = macro Impl.reflectorTrait
}

class C1(a: Int) {
}

trait T {
  def foo = 42
}

class Impl(val c: Context) {
  def reflectorConstructor(dynamic: c.Tree): c.Tree = {
    import c.universe._
    impl(dynamic, symbolOf[C1].info.decl(nme.CONSTRUCTOR))
  }
  def reflectorTrait(dynamic: c.Tree): c.Tree = {
    import c.universe._
    impl(dynamic, symbolOf[T].info.decl(TermName("foo")))
  }

  def reflector(dynamic: c.Tree): c.Tree = {
    impl(dynamic, c.internal.enclosingOwner)
  }

  private def impl(dynamic: c.Tree, reflectionSubject0: c.Symbol): c.Tree = {
    {
      val symtab = c.universe.asInstanceOf[SymbolTable]
      import symtab._
      val reflectionSubject             = reflectionSubject0.asInstanceOf[Symbol]
      val bootstrapMethod               = typeOf[test.Bootstrap].companion.member(TermName("bootstrap"))
      val paramSym                      = NoSymbol.newTermSymbol(TermName("x")).setInfo(typeOf[String])
      val dummySymbol                   = NoSymbol.newTermSymbol(TermName("reflector")).setInfo(internal.methodType(paramSym :: Nil, typeOf[String]))
      val reflectionSubjectParams       = reflectionSubject.info.paramss.flatten
      val bootstrapArgTrees: List[Tree] = List(
        Literal(Constant(bootstrapMethod)).setType(NoType),
        Literal(Constant(reflectionSubjectParams.length)).setType(typeOf[Int]),
        Literal(Constant(reflectionSubject)).setType(typeOf[MethodHandle])
        ) ::: reflectionSubjectParams.map(s => Literal(Constant(s.name.decoded)).setType(typeOf[String]))
      val result                        = ApplyDynamic(Ident(dummySymbol).setType(dummySymbol.info), bootstrapArgTrees ::: List(dynamic.asInstanceOf[symtab.Tree]))
      result.setType(dummySymbol.info.resultType)
      result.asInstanceOf[c.Tree]
    }
  }
}
