package scala.reflect.internal

import org.junit.Test

import scala.reflect.internal.util.NoSourceFile
import scala.tools.nsc.reporters.StoreReporter
import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import scala.tools.testing.AssertUtil

class PositionsTest {

  private object symbolTable extends SymbolTableForUnitTesting {
    override def useOffsetPositions: Boolean = false
    override val reporter = new StoreReporter(settings)
  }

  @Test def positionValidation(): Unit = {
    import symbolTable._
    def checkInvalid(tree: Tree): Unit = {
      reporter.reset()
      AssertUtil.assertThrows[ValidateException](validatePositions(tree))
    }

    def checkValid(tree: Tree): Unit = {
      reporter.reset()
      validatePositions(tree)
      assert(!reporter.hasErrors)
    }
    def rangePos(start: Int, end: Int): util.Position = util.Position.range(NoSourceFile, start, start, end)
    def offsetPos(point: Int): util.Position = util.Position.offset(NoSourceFile, point)
    def tree: Tree = Ident(TermName("x"))
    def rangePositioned(start: Int, end: Int): Tree = {
      Ident(TermName("x")).setPos(rangePos(start, end))
    }
    // overlapping ranges
    checkInvalid(Block(rangePositioned(0, 2), rangePositioned(1, 2), EmptyTree).setPos(rangePos(0, 2)))
    checkInvalid(Block(rangePositioned(1, 2), rangePositioned(0, 2), EmptyTree).setPos(rangePos(0, 2)))

    // transparent position not deemed to overlap itself
    checkValid(Block(rangePositioned(0, 2), tree.setPos(rangePos(1, 2).makeTransparent), EmptyTree).setPos(rangePos(0, 2)))

    // children of transparent position overlapping with sibling of transparent position.
    checkInvalid(Block(rangePositioned(0, 2), Block(Nil, rangePositioned(1, 2)).setPos(rangePos(1, 2).makeTransparent), EmptyTree).setPos(rangePos(0, 2)))

    // adjacent ranges are allowed to touch
    checkValid(Block(rangePositioned(0, 1), rangePositioned(1, 2), EmptyTree).setPos(rangePos(0, 2)))

    // offset position between overlapping ranges
    checkInvalid(Block(rangePositioned(0, 2), tree.setPos(offsetPos(0)), rangePositioned(1, 2), EmptyTree).setPos(rangePos(0, 2)))

    // child range position larger than parent
    checkInvalid(Block(Nil, rangePositioned(0, 3)).setPos(rangePos(0, 2)))

    // child offset position outside of parent
    checkInvalid(Block(Nil, tree.setPos(offsetPos(3)).setPos(rangePos(0, 2))))
  }
}
