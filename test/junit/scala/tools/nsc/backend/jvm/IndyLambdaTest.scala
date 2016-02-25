package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.{Assert, Test}

import scala.tools.asm.{Handle, Opcodes}
import scala.tools.asm.tree.InvokeDynamicInsnNode
import scala.tools.nsc.backend.jvm.AsmUtils._
import scala.tools.nsc.backend.jvm.CodeGenTools._
import scala.tools.testing.ClearAfterClass
import scala.collection.JavaConverters._

object IndyLambdaTest extends ClearAfterClass.Clearable {
  var compiler = newCompiler()

  def clear(): Unit = {
    compiler = null
  }
}

class IndyLambdaTest extends ClearAfterClass {
  ClearAfterClass.stateToClear = IndyLambdaTest
  val compiler = IndyLambdaTest.compiler

  @Test def boxingBridgeMethodUsedSelectively(): Unit = {
    def implMethodDescriptorFor(code: String): String = {
      val method = compileMethods(compiler)(s"""def f = $code """).find(_.name == "f").get
      val x = method.instructions.iterator.asScala.toList
      x.flatMap {
        case insn : InvokeDynamicInsnNode => insn.bsmArgs.collect { case h : Handle => h.getDesc }
        case _ => Nil
      }.head
    }
    // unspecialized functions that have a primitive in parameter or return position
    // give rise to a "boxing bridge" method (which has the suffix `$adapted`).
    // This is because Scala's unboxing of null values gives zero, whereas Java's throw a NPE.

    // 1. Here we show that we are calling the boxing bridge (the lambda bodies here are compiled into
    //    methods of `(I)Ljava/lang/Object;` / `(I)Ljava/lang/Object;` respectively.)
    assertEquals("(Ljava/lang/Object;)Ljava/lang/Object;", implMethodDescriptorFor("(x: Int) => new Object"))
    assertEquals("(Ljava/lang/Object;)Ljava/lang/Object;", implMethodDescriptorFor("(x: Object) => 0"))

    // 2a. We don't need such adaptations for parameters or return values with types that differ
    // from Object due to other generic substitution, LambdaMetafactory will downcast the arguments.
    assertEquals("(Ljava/lang/String;)Ljava/lang/String;", implMethodDescriptorFor("(x: String) => x"))

    // 2b. Testing 2a. in combination with 1.
    assertEquals("(Ljava/lang/Object;)Ljava/lang/String;", implMethodDescriptorFor("(x: Int) => \"\""))
    assertEquals("(Ljava/lang/String;)Ljava/lang/Object;", implMethodDescriptorFor("(x: String) => 0"))

    // 3. Specialized functions, don't need any of this as they implement a method like `apply$mcII$sp`,
    //    and the (un)boxing is handled in the base class in code emitted by scalac.
    assertEquals("(I)I", implMethodDescriptorFor("(x: Int) => x"))
  }
}
