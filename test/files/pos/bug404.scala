trait AbsIterator {
  type T;
  def next: T;
}
class StringIterator extends AbsIterator {
  type T = char;
  def next = 'a';
}
trait SyncIterator extends AbsIterator {
  abstract override def next: T = super.next;
}
class I extends StringIterator with SyncIterator;
