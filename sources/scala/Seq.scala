package scala;

trait Seq[+a] with Function1[Int, a] with Iterable[a] {
  def length: Int;
  def elements: Iterator[a];
  def iterator: Iterator[a] = elements;
  def apply(index: Int): a;

  /** @deprecated
   */
  def at(index: Int): a;
}
