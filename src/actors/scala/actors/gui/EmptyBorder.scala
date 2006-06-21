package scala.actors.gui;

import javax.swing._

class EmptyBorder(_top: int, _left: int, _bottom: int, _right: int)
extends border.EmptyBorder(_top, _left, _bottom, _right) {
  def this() = this(0, 0, 0, 0)
}
