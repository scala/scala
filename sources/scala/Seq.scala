package scala;

trait Seq[+a] extends scala.Object with Function1[Int, a] {
  def length: Int;
  def elements: Iterator[a];
  def apply(index: Int): a;

  /** @deprecated
   */
  def at(index: Int): a;
}
