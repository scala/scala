package scala.tools.nsc.typechecker

import org.junit.Assert.{assertEquals, assertTrue}
import org.junit.Test

import scala.annotation.nowarn
import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.{Global, Settings}

class NotAMemberErrorTest {
  /** Counts how often the hints of `NotAMemberError` are computed, via `linePrecedes`, which they always call. */
  class CountingGlobal(settings: Settings, reporter: StoreReporter) extends Global(settings, reporter) {
    var hintsComputed = 0
    @nowarn("cat=deprecation") // early initializer, as in Global.analyzer
    override lazy val analyzer = new { val global: CountingGlobal.this.type = CountingGlobal.this } with AbstractAnalyzer {
      override def linePrecedes(t1: global.Tree, t2: global.Tree): Boolean = {
        hintsComputed += 1
        super.linePrecedes(t1, t2)
      }
    }
  }

  def compile(code: String): (CountingGlobal, StoreReporter) = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val reporter = new StoreReporter(settings)
    val global = new CountingGlobal(settings, reporter)
    new global.Run().compileSources(List(new BatchSourceFile("Test.scala", code)))
    (global, reporter)
  }

  @Test def hintsNotComputedForDiscardedError(): Unit = {
    // `i += 1` is first typed as `i.+=(1)`, which fails silently before falling back to `i = i + 1`.
    val (global, reporter) = compile("class C { var i = 0; def f(): Unit = i += 1 }")
    assertTrue(reporter.infos.mkString("\n"), reporter.infos.isEmpty)
    assertEquals(0, global.hintsComputed)
  }

  @Test def hintsComputedForReportedError(): Unit = {
    val (global, reporter) = compile("class C { def f(xs: List[Int]) = xs.lenght }")
    val messages = reporter.infos.toList.map(_.msg)
    assertEquals(List("value lenght is not a member of List[Int]\ndid you mean length? or perhaps lengthIs?"), messages)
    assertEquals(1, global.hintsComputed)
  }
}
