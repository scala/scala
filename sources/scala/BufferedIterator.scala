package scala;

trait BufferedIterator[+b] extends Iterator[b] {
  def head: b;
}
