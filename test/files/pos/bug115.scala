class S[A](f: A => A, x: A) {
  System.out.println(f(x));
}
class T[A](f: A => A, y: A) extends S(x: A => f(x), y) {
}
object Test with Application {
  new T[int](x: int => x * 2, 1);
}
