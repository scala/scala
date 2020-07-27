package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.tools.testing.BytecodeTesting

class IndyLambdaDirectTest extends BytecodeTesting {
  import compiler._
  override def compilerArgs = "-Ydelambdafy:method-ref"

  @Test def f0(): Unit = {
    compileToBytes("object F0 { def f0(f: Function0[String]) = () }")
    val meths = compileAsmMethods("def f() = F0.f0(() => toString())").map(_.name)
    assertTrue(s"Expected no anonfuns: $meths", meths.forall(!_.contains("anonfun")))
  }

  @Test def f1(): Unit = {
    compileToBytes("object F1 { def f1(f: Function1[Any, String]) = () }")
    val meths = compileAsmMethods("def f() = F1.f1(_.toString())").map(_.name)
    assertTrue(s"Expected no anonfuns: $meths", meths.forall(!_.contains("anonfun")))
  }
}
