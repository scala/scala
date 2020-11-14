package scala

import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testkit.BytecodeTesting

@RunWith(classOf[JUnit4])
class CrossTalkTest extends BytecodeTesting {
  import compiler.compileClasses

  @Test def eins(): Unit = compileClasses("class A; class B  extends A; class C extends B ")
  @Test def zwei(): Unit = compileClasses("         trait B  extends A; class C extends B ", List("interface A {}" -> "A.java"))
  @Test def zwey(): Unit = compileClasses("         trait B1 extends A; class C extends B1", List("interface A {}" -> "A.java"))

  @Test def a(): Unit = compileClasses("                   class C extends T", List("interface T {}" -> "T.java"))
  @Test def b(): Unit = compileClasses("trait T extends A; class C extends T", List("interface A {}" -> "A.java"))
}
