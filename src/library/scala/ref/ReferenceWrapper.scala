package scala.ref;

trait ReferenceWrapper[+T <: AnyRef] extends Reference[T] {
  val underlying : java.lang.ref.Reference;
  def isValid = underlying.get != null;
  def apply() = {
    val ret = underlying.get.asInstanceOf[T];
    if (ret eq null) throw new NoSuchElementException;
    ret;
  }
  def clear = underlying.clear;
  def enqueue = underlying.enqueue;
  def isEnqueued = underlying.isEnqueued;
}
