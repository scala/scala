package scala {

trait Iterator[a] {
  def hasNext: Boolean;
  def next: a;
  def filter(p: a => Boolean): Iterator[a] = new BufferedIterator[a] with {
    private val source     = new Buffered(Iterator.this);
    private def skip: Unit = while (source.hasNext && !p(source.head)) { source.next; () }
    def hasNext: Boolean   = { skip; source.hasNext }
    def next: a            = { skip; source.next }
    def head: a            = { skip; source.head; }
  }
}

trait BufferedIterator[a] extends Iterator[a] {
  def head: a;
}

class Buffered[a](it: Iterator[a]) extends Iterator[a] {
  private var hd: a = _;
  private var ahead: Boolean = False;
  def head: a = {
    if (!ahead) { hd = it.next; ahead = True }
    hd
  }
  def next: a =
    if (ahead) { ahead = False; hd }
    else head;

  def hasNext: Boolean =
    ahead || it.hasNext;
}
}


