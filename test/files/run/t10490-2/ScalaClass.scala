/* Similar to t10490 -- but defines `Foo` in the object.
 * Placing this test within t10490 makes it work without a fix, that's why it's independent.
 * Note that this was already working, we add it to make sure we don't regress
 */

class Foo
object Foo {
  class Bar {
    override def toString: String = "Foo$Bar was instantiated!"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    // JavaClass is the user of the Scala defined classes
    println(JavaClass.bar)
  }
}