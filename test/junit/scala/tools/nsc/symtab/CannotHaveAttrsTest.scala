package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4

import scala.tools.testing.assertThrows
import scala.reflect.internal.util.OffsetPosition

@RunWith(classOf[JUnit4])
class CannotHaveAttrsTest {
  class CustomSymbolTable extends SymbolTableForUnitTesting {
    object CHA extends CannotHaveAttrs {
      def canEqual(that: Any): Boolean = ???
      def productArity: Int = ???
      def productElement(n: Int): Any = ???
    }
    val attrlessTrees = List(CHA, EmptyTree, emptyValDef, pendingSuperCall)
  }
  def withCtx(body: CustomSymbolTable => Unit) = body(new CustomSymbolTable)

  @Test
  def canHaveAttrsIsFalse = withCtx { st => import st._
    attrlessTrees.foreach { t =>
      assertFalse(t.canHaveAttrs)
    }
  }

  @Test
  def defaultPosAssignment = withCtx { st => import st._
    attrlessTrees.foreach { t =>
      assertEquals(t.pos, NoPosition)
      t.pos = NoPosition
      assertEquals(t.pos, NoPosition)
      t.setPos(NoPosition)
      assertEquals(t.pos, NoPosition)
    }
  }

  @Test
  def defaultTpeAssignment = withCtx { st => import st._
    attrlessTrees.foreach { t =>
      assertEquals(t.tpe, NoType)
      t.tpe = NoType
      assertEquals(t.tpe, NoType)
      t.setType(NoType)
      assertEquals(t.tpe, NoType)
    }
  }

  @Test
  def nonDefaultPosAssignmentFails = withCtx { st => import st._
    val pos = new OffsetPosition(null, 0)
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.pos = pos }
      assertThrows[IllegalArgumentException] { t.setPos(pos) }
    }
  }

  @Test
  def nonDefaultTpeAssignmentFails = withCtx { st => import st._
    val tpe = typeOf[Int]
    attrlessTrees.foreach { t =>
      assertThrows[IllegalArgumentException] { t.tpe = tpe }
      assertThrows[IllegalArgumentException] { t.setType(tpe) }
    }
  }
}
