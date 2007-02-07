package scala.ref;

class SoftReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  def this(value : T) = this(value, null);
  val underlying =
    if (queue == null) new java.lang.ref.SoftReference(value);
    else new java.lang.ref.SoftReference(value, queue.underlying);
}
