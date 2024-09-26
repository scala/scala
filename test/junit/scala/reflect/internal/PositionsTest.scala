package scala.reflect.internal

import org.junit.Test
import org.junit.Assert.assertFalse

import scala.reflect.internal.util.{NoSourceFile, Position}
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testkit.AssertUtil.assertThrows

class PositionsTest {

  object symbolTable extends SymbolTableForUnitTesting {
    override def useOffsetPositions: Boolean = false
    override val reporter = new StoreReporter(settings)
  }
  import symbolTable._

  def sentinel = Literal(Constant(())).setPos(offsetPos(100))

  def checkInvalid(tree: Tree): Unit = {
    reporter.reset()
    assertThrows[ValidateException](validatePositions(tree))
  }

  def checkValid(tree: Tree): Unit = {
    reporter.reset()
    try validatePositions(tree)
    finally reporter.infos.foreach(println)
    assertFalse(reporter.hasErrors)
  }

  def checkInvalid(pos: Position)(trees: Tree*): Unit =
    if (trees.length == 1) checkInvalid(Block(stats = Nil, expr = trees.head).setPos(pos))
    else checkInvalid(Block(stats = trees.toList, expr = sentinel).setPos(pos))

  def checkValid(pos: Position)(trees: Tree*): Unit =
    if (trees.length == 1) checkValid(Block(stats = Nil, expr = trees.head).setPos(pos))
    else checkValid(Block(stats = trees.toList, expr = sentinel).setPos(pos))

  def rangePos(start: Int, end: Int): Position = Position.range(NoSourceFile, start, start, end)
  def offsetPos(point: Int): Position = Position.offset(NoSourceFile, point)
  def x: Tree = Ident(TermName("x"))
  def rangePositioned(start: Int, end: Int): Tree = x.setPos(rangePos(start, end))

  //@Test
  def positionValidation: Unit = {
    // overlapping ranges
    checkInvalid(rangePos(0, 2))(rangePositioned(0, 2), rangePositioned(1, 2))
    checkInvalid(rangePos(0, 2))(rangePositioned(1, 2), rangePositioned(0, 2))

    // transparent position not deemed to overlap itself
    checkValid(rangePos(0, 2))(rangePositioned(0, 2), x.setPos(rangePos(1, 2).makeTransparent))

    // children of transparent position overlapping with sibling of transparent position.
    checkInvalid(rangePos(0, 2))(rangePositioned(0, 2), Block(Nil, rangePositioned(1, 2)).setPos(rangePos(1, 2).makeTransparent))

    // adjacent ranges are allowed to touch
    checkValid(rangePos(0, 2))(rangePositioned(0, 1), rangePositioned(1, 2))

    // offset position between overlapping ranges
    checkInvalid(rangePos(0, 2))(rangePositioned(0, 2), x.setPos(offsetPos(0)), rangePositioned(1, 2))

    // child range position larger than parent
    checkInvalid(rangePos(0, 2))(rangePositioned(0, 3))

    // child offset position outside of parent
    checkInvalid(rangePos(0, 2))(x.setPos(offsetPos(3)))
  }

  @Test def `focused position is synthetic`: Unit = checkValid(rangePos(27, 42)) {
    x.setPos(offsetPos(3))
  }
}
