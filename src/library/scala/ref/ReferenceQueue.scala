package scala.ref;

class ReferenceQueue[+T <: AnyRef] {
  private[ref] val underlying = new java.lang.ref.ReferenceQueue;
  override def toString = underlying.toString;
  class Wrapper(val underlying : java.lang.ref.Reference) extends ReferenceWrapper[T];
  def Wrapper(ref : java.lang.ref.Reference) = ref match {
    case null => None;
    case ref => new Wrapper(ref);
  }
  def poll = Wrapper(underlying.poll);
  def remove = Wrapper(underlying.remove);
  def remove(timeout : Long) = Wrapper(underlying.remove(timeout));
}
