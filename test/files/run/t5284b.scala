





/** Here we have a situation where a normalized method parameter `W`
 *  is used in a position which expects a type `T` - we know we can
 *  safely cast `W` to `T` whenever typebounds of `W` hold.
 */
object Test {
  def main(args: Array[String]) {
    val foo = Foo.createUnspecialized[Int]
    println(foo.bar(17))
  }
}


object Foo {
  def createUnspecialized[T] = new Foo[T]
}


class Foo[@specialized(Int) T] {
  val id: T => T = x => x

  def bar[@specialized(Int) W <: T, @specialized(Int) S](w: W) = id(w)
}
