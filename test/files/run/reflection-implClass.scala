/** 
 * Tries to load a symbol for the `Foo$class` using Scala reflection.  
 * Since trait implementation classes do not get pickling information
 * symbol for them should be created using fallback mechanism
 * that exposes Java reflection information dressed up in
 * a Scala symbol.
 **/
object Test extends App {
  import scala.reflect.mirror
  val name = manifest[Foo].erasure.getName + "$class"
  val implClass = Class.forName(name)
  val symbol = mirror.classToSymbol(implClass)
  assert(symbol != mirror.NoSymbol)
}

trait Foo {
  def bar = 1
}
