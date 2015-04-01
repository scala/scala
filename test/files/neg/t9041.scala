// False negative test, requires overloading in Cell.

trait Cell { def setCellValue(i: Int) = () ; def setCellValue(d: Double) = () }

trait Nope {
  def f = {
    trait CellSetter[A] {
      def setCell(cell: Cell, data: A): Unit
    }
    implicit val bigDecimalCellSetter = new CellSetter[math.BigDecimal]() {
      def setCell(cell: Cell, data: math.BigDecimal) { cell.setCellValue(data) }
    }
    implicit class RichCell(cell: Cell) {
      def setCellValue[A](data: A)(implicit cellSetter: CellSetter[A]) = cellSetter.setCell(cell, data)
    }
  }
}
