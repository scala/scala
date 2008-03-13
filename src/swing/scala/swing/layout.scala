package swing

import java.awt._

object layout {

  val flex = 0

  def grid(rows: int, columns: int) = new GridLayout(rows, columns)
  def row = new FlowLayout()
  def column = grid(flex, 1)
}
