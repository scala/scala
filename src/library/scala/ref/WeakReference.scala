package scala.ref;

class WeakReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value : T) = this(value, null);
  val underlying =
    if (queue == null) new java.lang.ref.WeakReference(value);
    else new java.lang.ref.WeakReference(value, queue.underlying);
}
