package scala;

final class EmptyStream[c]() extends Stream[c] with {
  def isEmpty = True;
  def head: c = error("head of empty stream");
  def tail: Stream[c] = error("tail of empty stream");
  override def toString(): String = "EmptyStream";
}

