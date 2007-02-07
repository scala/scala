package scala.ref;

class PhantomReference[+T <: AnyRef](value : T, queue : ReferenceQueue[T]) extends ReferenceWrapper[T] {
  val underlying = new java.lang.ref.PhantomReference(value, queue.underlying);
}
