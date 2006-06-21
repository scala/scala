package scala.actors.gui;

import javax.swing

class Caret(val jcaret: swing.text.Caret) {
  def dot: int = jcaret.getDot()
  def mark: int = jcaret.getMark()
}
