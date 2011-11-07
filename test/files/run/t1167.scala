/** Tests for compatible InnerClasses attribute between trait and
 *  impl classes, as well as anonymous classes.
 */

trait Test1 {
  def testFunc(i:Int): Unit = {
    (i:Int) => i + 5
  }
}
	
abstract class Foo {
  override def toString = getClass.getSimpleName
  
  abstract class Bar {
    override def toString = getClass.getSimpleName
  } 
}

object Test extends Application {
  val foo = new Foo {}
  val bar = new foo.Bar {}
  println(foo)
  println(bar)
  println(Class.forName("Test1$$anonfun$testFunc$1").getSimpleName)
}
