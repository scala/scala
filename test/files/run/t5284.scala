import scala.reflect.ClassTag




/** Here we have a situation where a normalized method parameter `W`
 *  is used in a position which accepts an instance of type `T` - we know we can
 *  safely cast `T` to `W` whenever type bounds on `W` hold.
 */
object Test {
  def main(args: Array[String]): Unit = {
    val a = Blarg(Array(1, 2, 3))
    println(a.m((x: Int) => x + 1))
  }
}


object Blarg {
  def apply[T: ClassTag](a: Array[T]) = new Blarg(a)
}


class Blarg[@specialized(Int) T: ClassTag](val a: Array[T]) {
  def m[@specialized(Int) W >: T, @specialized(Int) S](f: W => S) = f(a(0))
}
