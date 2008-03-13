package swing;

import javax.swing

class Caret(val jcaret: swing.text.Caret) extends Publisher {
  def dot: int = jcaret.getDot()
  def mark: int = jcaret.getMark()
}
