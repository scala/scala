class S[A](f: A => A, x: A) {
  System.out.println(f(x));
}
class T[B](f: B => B, y: B) extends S(x: B => f(x), y) {
}
object Test with Application {
  new T[Int](x => x * 2, 1);
  val f = new S(x: Int => x, 1);
}
