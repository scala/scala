class B[T] {
  def foo(x: T): Int = 1;
}
trait J[T] {
  def foo(x: T): Int;
}
trait I[T] extends B[T] with J[T] {
  abstract override def foo(x: T): Int = super[J].foo(x) + 1;
}
class C extends B[Int] {
  override def foo(x: Int): Int = x;
}
class D extends C with I[Int];
object T with Application {
  System.out.println((new D).foo(3));
}

