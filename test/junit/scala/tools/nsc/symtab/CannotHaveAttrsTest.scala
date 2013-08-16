package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.AssertUtil.assertThrows
import scala.reflect.internal.util.OffsetPosition

@RunWith(classOf[JUnit4])
class CannotHaveAttrsTest {
  object symbolTable extends SymbolTableForUnitTesting {
    object CHA extends CannotHaveAttrs {
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }
    val attrlessTrees = List(CHA, EmptyTree, emptyValDef, pendingSuperCall)
  }
  import symbolTable._

  @Test
  def canHaveAttrsIsFalse =
    attrlessTrees.foreach { t =>
      assertFalse(t.canHaveAttrs)
    }

  @Test
  def defaultPosAssignment =
    attrlessTrees.foreach { t =>
      assertEquals(t.pos, NoPosition)
      t.pos = NoPosition
      assertEquals(t.pos, NoPosition)
      t.setPos(NoPosition)
      assertEquals(t.pos, NoPosition)
    }

  @Test
  def defaultTpeAssignment =
    attrlessTrees.foreach { t =>
      assertEquals(t.tpe, NoType)
      t.tpe = NoType
      assertEquals(t.tpe, NoType)
      t.setType(NoType)
      assertEquals(t.tpe, NoType)
    }

  @Test
  def nonDefaultPosAssignmentFails = {
    val pos = new OffsetPosition(null, 0)
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.pos = pos }
      assertThrows[IllegalArgumentException] { t.setPos(pos) }
    }
  }

  @Test
  def nonDefaultTpeAssignmentFails = {
    val tpe = typeOf[Int]
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.tpe = tpe }
      assertThrows[IllegalArgumentException] { t.setType(tpe) }
    }
  }
}
