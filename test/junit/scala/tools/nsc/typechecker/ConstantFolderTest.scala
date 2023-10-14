package scala.tools.nsc
package typechecker

import org.junit.Test
import org.junit.Assert.assertTrue

import scala.tools.testkit.BytecodeTesting

class ConstantFolderTest extends BytecodeTesting {
  import compiler._
  import global._

  override def compilerArgs = "-Ystop-after:typer"

  def literalValDefAssert(tree: Tree, name: String, constant: Constant): Unit = {
    val valDef: ValDef = tree.collect {
      case node @ ValDef(_, nm, _, _) if nm.decoded.trim == name => node
    }.head

    assertTrue(s"Expected val $name: $constant", valDef.collect { case node @ Literal(`constant`) => node }.nonEmpty)
  }

  @Test
  def testStringFolder(): Unit = {
    val code =
      """object O {
        |  final val x = "123" + "Scala"
        |}
      """.stripMargin
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    literalValDefAssert(tree, "x", Constant("123Scala"))
  }

  @Test
  def testBooleanFolder(): Unit = {
    val code =
      """object O {
        |  final val x0 = true && true
        |  final val x1 = true && false
        |  final val x2 = false && true
        |  final val x3 = false && false
        |  final val x4 = true || true
        |  final val x5 = true || false
        |  final val x6 = false || true
        |  final val x7 = false || false
        |  final val x8 = !x7
        |}
      """.stripMargin
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    literalValDefAssert(tree, "x0", Constant(true))
    literalValDefAssert(tree, "x1", Constant(false))
    literalValDefAssert(tree, "x2", Constant(false))
    literalValDefAssert(tree, "x3", Constant(false))
    literalValDefAssert(tree, "x4", Constant(true))
    literalValDefAssert(tree, "x5", Constant(true))
    literalValDefAssert(tree, "x6", Constant(true))
    literalValDefAssert(tree, "x7", Constant(false))
    literalValDefAssert(tree, "x8", Constant(true))
  }

  @Test def `fold unary ops`: Unit = {
    val code =
      sm"""|object X {
           |  final val x0 = 0xff
           |  final val x1 = ~0xff
           |  final val x2 = x0 & ~0xf
           |  final val x3 = x0.toLong
           |}"""
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    literalValDefAssert(tree, "x0", Constant(0xff))
    literalValDefAssert(tree, "x1", Constant(0xffffff00))
    literalValDefAssert(tree, "x2", Constant(0xf0))
    literalValDefAssert(tree, "x3", Constant(0xffL))
  }
}
