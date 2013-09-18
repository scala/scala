





/** Here we have a compound type `List[W]` used in
 *  a position where `List[T]` is expected. The cast
 *  emitted in the normalized `bar` is safe because the
 *  normalized `bar` can only be called if the type
 *  bounds hold.
 */
object Test {
  def main(args: Array[String]) {
    val foo = Foo.createUnspecialized[Int]
    println(foo.bar(List(1, 2, 3)))
  }
}


object Foo {
  def createUnspecialized[T] = new Foo[T]
}


class Foo[@specialized(Int) T] {
  val len: List[T] => Int = xs => xs.length

  def bar[@specialized(Int) W <: T](ws: List[W]) = len(ws)
}
