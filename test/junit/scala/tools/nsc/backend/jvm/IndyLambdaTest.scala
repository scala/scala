package scala.tools.nsc.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.collection.JavaConverters._
import scala.tools.asm.Handle
import scala.tools.asm.tree.InvokeDynamicInsnNode
import scala.tools.testing.BytecodeTesting

class IndyLambdaTest extends BytecodeTesting {
  import compiler._

  @Test def boxingBridgeMethodUsedSelectively(): Unit = {
    def implMethodDescriptorFor(code: String): String = {
      val method = compileAsmMethods(s"""def f = $code """).find(_.name == "f").get
      val x = method.instructions.iterator.asScala.toList
      x.flatMap {
        case insn : InvokeDynamicInsnNode => insn.bsmArgs.collect { case h : Handle => h.getDesc }
        case _ => Nil
      }.head
    }

    val obj = "Ljava/lang/Object;"
    val str = "Ljava/lang/String;"

    // unspecialized functions that have a primitive in parameter or return position
    // give rise to a "boxing bridge" method (which has the suffix `$adapted`).
    // This is because Scala's unboxing of null values gives zero, whereas Java's throw a NPE.

    // 1. Here we show that we are calling the boxing bridge (the lambda bodies here are compiled into
    //    methods of `(I)Ljava/lang/Object;` / `(I)Ljava/lang/Object;` respectively.)
    assertEquals(s"($obj)$obj", implMethodDescriptorFor("(x: Int) => new Object"))
    assertEquals(s"($obj)$obj", implMethodDescriptorFor("(x: Object) => 0"))

    // 2a. We don't need such adaptations for parameters or return values with types that differ
    // from Object due to other generic substitution, LambdaMetafactory will downcast the arguments.
    assertEquals(s"($str)$str", implMethodDescriptorFor("(x: String) => x"))

    // 2b. Testing 2a. in combination with 1.
    assertEquals(s"($obj)$str", implMethodDescriptorFor("(x: Int) => \"\""))
    assertEquals(s"($str)$obj", implMethodDescriptorFor("(x: String) => 0"))

    // 3. Specialized functions, don't need any of this as they implement a method like `apply$mcII$sp`,
    //    and the (un)boxing is handled in the base class in code emitted by scalac.
    assertEquals("(I)I", implMethodDescriptorFor("(x: Int) => x"))

    // non-builtin sams are like specialized functions
    compileToBytes("class VC(private val i: Int) extends AnyVal; trait FunVC { def apply(a: VC): VC }")
    assertEquals("(I)I", implMethodDescriptorFor("((x: VC) => x): FunVC"))

    compileToBytes("trait Fun1[T, U] { def apply(a: T): U }")
    assertEquals(s"($obj)$str", implMethodDescriptorFor("(x => x.toString): Fun1[Int, String]"))
    assertEquals(s"($obj)$obj", implMethodDescriptorFor("(x => println(x)): Fun1[Int, Unit]"))
    assertEquals(s"($obj)$str", implMethodDescriptorFor("((x: VC) => \"\") : Fun1[VC, String]"))
    assertEquals(s"($str)$obj", implMethodDescriptorFor("((x: String) => new VC(0)) : Fun1[String, VC]"))

    compileToBytes("trait Coll[A, Repr] extends Any")
    compileToBytes("final class ofInt(val repr: Array[Int]) extends AnyVal with Coll[Int, Array[Int]]")

    assertEquals(s"([I)$obj", implMethodDescriptorFor("((xs: Array[Int]) => new ofInt(xs)): Array[Int] => Coll[Int, Array[Int]]"))
  }
}
