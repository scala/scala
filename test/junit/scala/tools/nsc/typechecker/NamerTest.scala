package scala.tools.nsc.typechecker

import org.junit._, Assert.assertEquals

import scala.annotation.nowarn
import scala.tools.testkit.BytecodeTesting

class NamerTest extends BytecodeTesting {

  import compiler.global._

  override def compilerArgs: String = "-Ystop-after:typer"

  @Test
  def defaultMethodsInDeclarationOrder(): Unit = {
    compiler.compileClasses("package p1; class Test { C.b(); C.a() }; object C { def a(x: Int = 0) = 0; def b(x: Int = 0) = 0 }"): @nowarn("cat=w-flag-value-discard")
    val methods = compiler.global.rootMirror.getRequiredModule("p1.C").info.decls.toList.map(_.name.toString).filter(_.matches("""(a|b).*"""))
    def getterName(s: String) = nme.defaultGetterName(TermName(s), 1).toString
    assertEquals(List("a", getterName("a"), "b", getterName("b")), methods) // order no longer depends on order of lazy type completion :)
  }
}
