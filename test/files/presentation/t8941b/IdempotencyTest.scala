package scala.tools.nsc
package interactive
package tests.core

import scala.tools.nsc.interactive.InteractiveReporter
import scala.reflect.internal.util.SourceFile
import reporters.Reporter

/** Deterministically interrupts typechecking of `code` when a definition named
  * `MagicInterruptionMarker` is typechecked, and then performs a targeted
  * typecheck of the tree at the special comment marker marker
  */  
abstract class IdempotencyTest { self =>
  private val settings = new Settings
  settings.usejavacp.value = true

  private object Break extends scala.util.control.ControlThrowable

  private val compilerReporter: Reporter = new InteractiveReporter {
    override def compiler = self.compiler
  }

  object compiler extends Global(settings, compilerReporter) {
    override def checkForMoreWork(pos: Position): Unit = {
    }
    override def signalDone(context: Context, old: Tree, result: Tree): Unit = {
      // println("signalDone: " + old.toString.take(50).replaceAll("\n", "\\n"))
      if (!interrupted && lockedCount == 0 && interruptsEnabled && shouldInterrupt(result)) {
        interrupted = true
        val typed = typedTreeAt(markerPosition)
        checkTypedTree(typed)
        throw Break
      }
      super.signalDone(context, old, result)
    }

    // we're driving manually using our own thread, disable the check here.
    override def assertCorrectThread(): Unit = {}
  }

  import compiler._

  private var interrupted = false

  // Extension points
  protected def code: String
  protected def shouldInterrupt(tree: Tree): Boolean = {
    tree.symbol != null && tree.symbol.name.toString == "MagicInterruptionMarker"
  }
  protected def checkTypedTree(tree: Tree): Unit = {}
  

  private val source: SourceFile = newSourceFile(code)
  private def markerPosition: Position = source.position(code.indexOf("/*?*/"))

  def assertNoProblems(): Unit = {
    val problems = getUnit(source).get.problems
    assert(problems.isEmpty, problems.mkString("\n"))
  }

  def show(): Unit = {
    reloadSource(source)
    try {
      typedTree(source, true)
      assert(false, "Expected to break out of typechecking.")
    } catch {
      case Break => // expected
    }
    assertNoProblems()
  }

  def main(args: Array[String]): Unit = { show() }
}
