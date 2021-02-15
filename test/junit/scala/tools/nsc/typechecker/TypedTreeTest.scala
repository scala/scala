package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test

import scala.tools.testing.BytecodeTesting

class TypedTreeTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  @Test
  def constantFoldedOriginalTreeAttachment(): Unit = {
    val code =
      """object O {
        |  final val x = 42
        |  def f(x: Int) = x
        |  def f(x: Boolean) = x
        |  f(O.x)
        |}
      """.stripMargin
    val run = compiler.newRun
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    val List(t) = tree.filter(_.attachments.all.nonEmpty).toList
    assertEquals(s"$t:${t.attachments.all}", "42:Set(OriginalTreeAttachment(O.x))")
  }


  // Ensure SingletonTypeTree#ref is typed and it has symbol after typing.
  // see: https://github.com/scala/bug/issues/12296
  @Test
  def singletonTypeTreeRefTyped(): Unit = {
    val code =
      """|object root {
         |  object impl
         |  val f: impl.type => Unit = {
         |    case _: impl.type => ()
         |  }
         |}
      """.stripMargin
    val run = compiler.newRun
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSingletonTypeTreeSource.scala")))
    val tree = run.units.next().body

    import compiler.global._

    val singletonTypeTrees = new collection.mutable.MutableList[SingletonTypeTree]
    object traverser extends Traverser {
      override def traverse(t: Tree): Unit = {
        t match {
          case tt: TypeTree if tt.original != null => traverse(tt.original)
          case st: SingletonTypeTree =>
            singletonTypeTrees += st
          case _ => super.traverse(t)
        }
      }
    }
    traverser.traverse(tree)

    singletonTypeTrees.foreach { t =>
      assertEquals(t.ref.symbol.fullName, "root.impl")
      assertNotEquals(NoPosition, t.pos)
    }
  }
}
