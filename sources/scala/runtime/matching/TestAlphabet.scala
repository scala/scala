package scala.runtime.matching ;

trait TestAlphabet;

case class TestLabel(i: Int) extends TestAlphabet ;

case object AnyNode extends TestAlphabet {
  def view(x: Int): TestLabel = TestLabel(x);
}
