package scala.swing

object EmptyBorder {
  def apply(top: int, left: int, bottom: int, right: int) =
    new javax.swing.border.EmptyBorder(top, left, bottom, right)
  def apply() = new javax.swing.border.EmptyBorder(0, 0, 0, 0)
}



