class S[A](f: A => A, x: A) {
  Console.println(f(x));
}
class T[B](f: B => B, y: B) extends S((x: B) => f(x), y) {
}
object Test extends App {
  new T[Int](x => x * 2, 1);
  val f = new S((x: Int) => x, 1);
}
