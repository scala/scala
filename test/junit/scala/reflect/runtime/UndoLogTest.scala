package scala.reflect.runtime

import org.junit.Assert._
import org.junit.Test

import scala.reflect.internal.SymbolTable
import scala.reflect.runtime.{universe => ru}

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
}
