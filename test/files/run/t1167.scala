/** Tests for compatible InnerClasses attribute between trait and
 *  impl classes, as well as anonymous classes.
 */

trait Test1 {
  def testFunc(i:Int): Unit = {
    (i:Int) => i + 5
  }
}

/* getName
 *   Returns the binary name of the class if this class object represents a
 *   reference type that is not an array type.
 * getSimpleName
 *   Returns the simple name of the underlying class as given in the source
 *   code. Returns an empty string if the underlying class is anonymous.
 */
abstract class Foo {
  override def toString = getClass.getSimpleName

  abstract class Bar {
    override def toString = getClass.getSimpleName
  }
}

object Test extends App {
  val foo = new Foo {}
  val bar = new foo.Bar {}
  println(foo)
  println(bar)
  println(Class.forName("Test1$$anonfun$testFunc$1").getSimpleName)
}
