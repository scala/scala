package scala;

trait Seq[a] {
  def length: Int;
  def elements: Iterator[a];
  def at(index: Int): a
}
