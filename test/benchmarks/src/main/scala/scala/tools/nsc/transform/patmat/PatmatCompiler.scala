package scala.tools.nsc.transform.patmat

import scala.reflect.internal.util.BatchSourceFile
import scala.tools.nsc.{Global, Phase, Settings}
import scala.tools.nsc.reporters.StoreReporter

/** Compiles up to and including `patmat`, with a new `Global` each time, and reports how long that
 *  phase took. */
final class PatmatCompiler(extraArgs: List[String] = Nil) {
  private val settings = new Settings()
  settings.usejavacp.value = true
  settings.processArguments("-Ystop-after:patmat" :: extraArgs, processAll = true)

  /** Compiles `code` and returns the nanoseconds spent in `patmat`. */
  def patmatNanos(code: String): Long = compile(code)._1

  /** Compiles `code` and returns the nanoseconds spent in `patmat`, and whether the analysis gave up
   *  ("Cannot check match", past `AnalysisBudget.maxFormulaSize`). */
  def compile(code: String): (Long, Boolean) = {
    val store = new StoreReporter(settings)
    val global = new Global(settings, store)
    import global._
    var start, end = 0L
    val run = new Run {
      override def informUnitStarting(phase: Phase, unit: CompilationUnit): Unit =
        if (phase.name == "patmat") start = System.nanoTime()
      override def advancePhase(): Unit = {
        if (globalPhase.prev.name == "patmat") end = System.nanoTime()
        super.advancePhase()
      }
    }
    run.compileSources(new BatchSourceFile("Shape.scala", code) :: Nil)
    if (store.hasErrors) throw new IllegalStateException(store.infos.mkString("compilation failed:\n", "\n", ""))
    (end - start, store.infos.exists(_.msg.startsWith("Cannot check match")))
  }
}
