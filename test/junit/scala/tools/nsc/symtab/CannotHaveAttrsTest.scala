package scala.tools.nsc
package symtab

import org.junit.jupiter.api.Assertions._
import org.junit.jupiter.api.{Disabled, Test}

import scala.reflect.internal.util.OffsetPosition
import scala.tools.testkit.AssertUtil.assertThrows

class CannotHaveAttrsTest {
  object symbolTable extends SymbolTableForUnitTesting {
    object CHA extends CannotHaveAttrs {
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }
    val attrlessTrees = List(CHA, EmptyTree, noSelfType, pendingSuperCall)
  }
  import symbolTable._

  @Test
  def canHaveAttrsIsFalse(): Unit =
    attrlessTrees.foreach { t =>
      assertFalse(t.canHaveAttrs)
    }

  @Test
  def defaultPosAssignment(): Unit =
    attrlessTrees.foreach { t =>
      assertEquals(t.pos, NoPosition)
      t.pos = NoPosition
      assertEquals(t.pos, NoPosition)
      t.setPos(NoPosition)
      assertEquals(t.pos, NoPosition)
    }

  @Test
  def defaultTpeAssignment(): Unit =
    attrlessTrees.foreach { t =>
      assertEquals(t.tpe, NoType)
      t.tpe = NoType
      assertEquals(t.tpe, NoType)
      t.setType(NoType)
      assertEquals(t.tpe, NoType)
    }

  @Test
  @Disabled("scala/bug#8816")
  def nonDefaultPosAssignmentFails(): Unit = {
    val pos = new OffsetPosition(null, 0)
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.pos = pos }
      assertThrows[IllegalArgumentException] { t.setPos(pos) }
    }
  }

  @Test
  @Disabled("scala/bug#8816")
  def nonDefaultTpeAssignmentFails(): Unit = {
    val tpe = typeOf[Int]
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.tpe = tpe }
      assertThrows[IllegalArgumentException] { t.setType(tpe) }
    }
  }

  class Attach
  @Test
  def attachmentsAreIgnored(): Unit = {
    attrlessTrees.foreach { t =>
      t.setAttachments(NoPosition.update(new Attach))
      assertEquals(NoPosition, t.attachments)
      t.updateAttachment(new Attach)
      assertEquals(NoPosition, t.attachments)
      t.removeAttachment[Attach] // no exception
    }
  }
}
