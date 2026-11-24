package scala.reflect.internal

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

import scala.tools.testkit.BytecodeTesting
import scala.tools.testkit.BytecodeTesting._

class TreeInfoTest {
  private val code =
    """object O { val member: String = "" }
      |class C {
      |  val member: String = ""
      |  lazy val lazyMember: String = ""
      |  def probe(name: String, a: Any): Unit = ()
      |  def test(param: String, byName: => String, other: C): Unit = {
      |    val local: String = ""
      |    var localVar: String = ""
      |    lazy val localLazy: String = ""
      |    object localObject
      |    def localDef: String = ""
      |    probe("literal", "")
      |    probe("this", this)
      |    probe("param", param)
      |    probe("local", local)
      |    probe("localVar", localVar)
      |    probe("typed", (local: Any))
      |    probe("byName", byName)
      |    probe("localLazy", localLazy)
      |    probe("localObject", localObject)
      |    probe("localDef", localDef)
      |    probe("member", member)
      |    probe("lazyMember", lazyMember)
      |    probe("other.member", other.member)
      |    probe("O.member", O.member)
      |    probe("O", O)
      |  }
      |  def matched(x: Any): String = x match { case s: String => s case _ => "" }
      |}
      |""".stripMargin

  private val safeToElide = Set("literal", "this", "param", "local", "localVar", "typed")

  private def check(after: String): Unit = {
    val compiler = newCompiler(extraArgs = s"-Ystop-after:$after")
    import compiler.global._
    val run = compiler.newRun()
    run.compileSources(List(BytecodeTesting.makeSourceFile(code, "TreeInfoTest.scala")))
    assertFalse(reporter.hasErrors)
    val body = run.units.next().body
    exitingPhase(run.phaseNamed(after)) {
      val probed = body.collect {
        case Apply(fun, List(Literal(Constant(name: String)), arg)) if fun.symbol.name.string_==("probe") => name -> treeInfo.isExprSafeToElide(arg)
      }
      assertEquals(after, 15, probed.size)
      assertEquals(after, safeToElide, probed.collect { case (name, true) => name }.toSet)

      // the result of a match is the parameter of its `matchEnd` label, which the pattern matcher leaves without an owner
      val matchResults = body.collect { case ld: LabelDef if ld.params.nonEmpty => ld.params.head }
      assertEquals(after, if (after == "typer") 0 else 1, matchResults.size)
      matchResults.foreach(res => assertTrue(s"$after: $res", treeInfo.isExprSafeToElide(res)))
    }
  }

  @Test def isExprSafeToElideAfterTyper(): Unit   = check("typer")
  @Test def isExprSafeToElideAfterUncurry(): Unit = check("uncurry")
  // erasure cannot be the last phase: it looks ahead to posterasure
  @Test def isExprSafeToElideAfterErasure(): Unit = check("posterasure")
}
