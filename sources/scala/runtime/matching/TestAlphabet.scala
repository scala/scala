package scala.runtime.matching ;

import scala.util.alphabet.{ AlphabetPlusWildcard, WildcardLabel };

trait TestAlphabet extends AlphabetPlusWildcard ;

case class TestLabel(i: Int) extends TestAlphabet ;

case object AnyNode extends TestAlphabet with WildcardLabel ;

object TestAlphabetView {
  def view(x: Int): TestLabel = TestLabel(x);
}
