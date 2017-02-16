trait T[A] {
  def a: A
  def foldLeft[B](zero: B, op: (B, A) => B): B = op(zero, a)
  def sum[B >: A](zero: B): B
}

class C[@specialized(Int) A](val a: A) extends T[A] {
  override def sum[@specialized(Int) B >: A](zero: B): B = foldLeft(zero, (x: B, y: B) => x)
}

object Test extends App {
  def factory[T](a: T): C[T] = new C[T](a)

  assert(new C[Int](1).sum(2) == 2)
  assert(new C[String]("ho").sum("hu") == "hu")
  assert(factory[Int](1).sum(2) == 2)
  assert(factory[String]("ho").sum("hu") == "hu")
}
