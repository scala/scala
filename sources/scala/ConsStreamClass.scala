package scala;

final class ConsStreamClass[b](hd: b, tl: () => Stream[b]) extends Stream[b] with {
  def isEmpty = False;
  def head = hd;
  private var tlVal: Stream[b] = null;
  private var tlDefined: Boolean = False;
  def tail: Stream[b] = {
    if (!tlDefined) { tlVal = tl(); tlDefined = True; }
    tlVal
  }
  override def toString(): String = "ConsStream(" + hd + ", ?)";
}
