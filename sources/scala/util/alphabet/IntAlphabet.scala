package scala.util.alphabet ;

trait IntAlphabet extends Alphabet ;

case class IntLabel(i: Int) extends IntAlphabet ;

object IntAlphabetView {
  def view(x: Int): IntLabel = IntLabel(x);
}
