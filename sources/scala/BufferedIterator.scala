package scala;

trait BufferedIterator[a] extends Iterator[a] {
  def head: a;
}
