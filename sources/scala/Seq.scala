package scala;

trait Seq[a] {
  def length: Int;
  def elements: Iterator[a];
}
