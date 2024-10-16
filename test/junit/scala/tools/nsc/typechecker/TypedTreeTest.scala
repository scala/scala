package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertNotEquals}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class TypedTreeTest extends BytecodeTesting {
  override def compilerArgs = "-Ystop-after:typer"

  @Test def t12703(): Unit = {
    import compiler._
    val a =
      """object Foo {
        |  sealed trait A
        |  sealed trait B
        |  final case class C(x: Int) extends A with B
        |}
        |""".stripMargin
    val b =
      """object T {
        |  import Foo._
        |  def f(a: A) = a match {
        |    case b: B => 0
        |    case _ => 1
        |  }
        |}
        |""".stripMargin

    def check(a: String, b: String) = {
      val r = newRun()
      r.compileSources(makeSourceFile(a, "A.scala") :: makeSourceFile(b, "B.scala") :: Nil)
      checkReport()
    }

    check(a, b)
    check(b, a)
  }

  @Test
  def keepBlockUndetparams(): Unit = {
    import compiler.global._
    val code =
      """class C {
        |  def f = Option(Map("a" -> "c")).getOrElse { println(); Map.empty } // was: Map[_ >: String with K, Any]
        |  def g = Option(Map("a" -> "c")).getOrElse { Map.empty }
        |}
        |""".stripMargin
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val t: Tree = run.units.next().body
    val c: Symbol = t.collect { case cd: ClassDef => cd.symbol }.head
    for (m <- List("f", "g"))
      assertEquals(c.info.member(TermName("f")).tpe.toString, "scala.collection.immutable.Map[String,String]")
  }

  @Test
  def constantFoldedOriginalTreeAttachment(): Unit = {
    import compiler.global._
    val code =
      """object O {
        |  final val x = 42       // accessor gets inlined constant
        |  def f(x: Int) = x
        |  def f(x: Boolean) = x
        |  f(O.x)                 // arg is inlined constant
        |}
      """.stripMargin
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSource.scala")))
    val tree = run.units.next().body
    tree.filter(_.hasAttachment[analyzer.OriginalTreeAttachment])
      .sortBy(_.pos.start)
      .toList
      .map(t => s"$t:${t.attachments.all}") match {
        case "42:Set(OriginalTreeAttachment(O.this.x))" :: "42:Set(OriginalTreeAttachment(O.x))" :: Nil =>
        case wrong => throw new MatchError(wrong)
      }
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
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "UnitTestSingletonTypeTreeSource.scala")))
    val tree = run.units.next().body

    import compiler.global._

    val singletonTypeTrees = collection.mutable.Buffer[SingletonTypeTree]()
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
