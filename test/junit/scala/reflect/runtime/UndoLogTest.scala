package scala.reflect.runtime

import org.junit.Assert._
import org.junit.Test

import scala.reflect.internal.SymbolTable
import scala.reflect.runtime.{universe => ru}
import scala.tools.nsc.{Global, Settings}

class UndoLogTest {
  private val st = ru.asInstanceOf[SymbolTable]
  import ru._

  private def assertNoConstraintsLeft(): Unit =
    assertEquals("undo log entries", 0, st.undoLog.log.size)

  // A successful check against an existential solves type variables for its quantifiers and
  // used to leave them in the undo log, which runtime reflection never cleared.
  @Test def successfulExistentialCheckLeavesNothingInTheUndoLog(): Unit = {
    assertTrue(typeOf[Map[String, Int]] <:< typeOf[Map[_, _]])
    assertNoConstraintsLeft()
    assertTrue(typeOf[java.util.ArrayList[String]] weak_<:< typeOf[java.util.List[_ <: CharSequence]])
    assertNoConstraintsLeft()
    lub(List(typeOf[Option[_ <: CharSequence]], typeOf[Some[String]]))
    assertNoConstraintsLeft()
  }

  @Test def failedExistentialCheckLeavesNothingInTheUndoLog(): Unit = {
    assertFalse(typeOf[List[String]] <:< typeOf[List[_ <: Int]])
    assertNoConstraintsLeft()
  }

  // `Implicits` holds a mark across several outermost checks and rolls back to it when a candidate
  // fails, so the log must survive those checks. Clearing it anyway breaks neg/t4457_1 and t4457_2.
  private def rollBackAcrossChecks(u: SymbolTable): Unit = {
    import u._
    val tv = TypeVar(definitions.ListClass.typeParams.head)
    undoLog.withMark { mark =>
      assertTrue(definitions.StringTpe <:< tv)
      assertTrue(definitions.IntTpe <:< tv)
      assertEquals(List(definitions.IntTpe, definitions.StringTpe), tv.constr.loBounds)
      undoLog.undoTo(mark)
    }
    assertEquals(Nil, tv.constr.loBounds)
    assertEquals("undo log entries", 0, undoLog.log.size)

    // Without a mark the log is cleared after each check, and nothing is left to roll back.
    val tv2 = TypeVar(definitions.ListClass.typeParams.head)
    assertTrue(definitions.StringTpe <:< tv2)
    assertEquals("undo log entries", 0, undoLog.log.size)
    assertEquals(List(definitions.StringTpe), tv2.constr.loBounds)
  }

  @Test def markHeldAcrossChecksRuntime(): Unit = rollBackAcrossChecks(st)

  @Test def markHeldAcrossChecksCompiler(): Unit = {
    val settings = new Settings
    settings.usejavacp.value = true
    val g = new Global(settings)
    new g.Run
    g.exitingTyper(rollBackAcrossChecks(g))
  }

  @Test def nestedMarks(): Unit = {
    import st._
    val tv = TypeVar(definitions.ListClass.typeParams.head)
    undoLog.withMark { outer =>
      assertTrue(definitions.StringTpe <:< tv)
      undoLog.undo(assertTrue(definitions.IntTpe <:< tv))
      assertEquals(List(definitions.StringTpe), tv.constr.loBounds)
      // the inner mark is released, but the outer one still keeps the log
      assertTrue(definitions.BooleanTpe <:< tv)
      undoLog.undoTo(outer)
    }
    assertEquals(Nil, tv.constr.loBounds)
  }

  @Test def markReleasedOnException(): Unit = {
    import st._
    try undoLog.withMark(_ => throw new RuntimeException) catch { case _: RuntimeException => }
    val tv = TypeVar(definitions.ListClass.typeParams.head)
    assertTrue(definitions.StringTpe <:< tv)
    assertEquals("undo log entries", 0, undoLog.log.size)
  }
}
