/** 
 * Tries to load a symbol for the `Foo$class` using Scala reflection.  
 * Since trait implementation classes do not get pickling information
 * symbol for them should be created using fallback mechanism
 * that exposes Java reflection information dressed up in
 * a Scala symbol.
 */
object Test extends App with Outer {
  import scala.reflect.mirror

  assert(mirror.classToSymbol(manifest[Foo].erasure).typeSig.declaration(mirror.newTermName("bar")).typeSig ==
    mirror.classToSymbol(manifest[Bar].erasure).typeSig.declaration(mirror.newTermName("foo")).typeSig)

  val s1 = implClass(manifest[Foo].erasure)
  assert(s1 != mirror.NoSymbol)
  assert(s1.typeSig != mirror.NoType)
  assert(s1.companionModule.typeSig != mirror.NoType)
  assert(s1.companionModule.typeSig.declaration(mirror.newTermName("bar")) != mirror.NoSymbol)
  val s2 = implClass(manifest[Bar].erasure)
  assert(s2 != mirror.NoSymbol)
  assert(s2.typeSig != mirror.NoType)
  assert(s2.companionModule.typeSig != mirror.NoType)
  assert(s2.companionModule.typeSig.declaration(mirror.newTermName("foo")) != mirror.NoSymbol)
  def implClass(clazz: Class[_]) = {
    val implClass = Class.forName(clazz.getName + "$class")
    mirror.classToSymbol(implClass)
  }
}

trait Foo {
  def bar = 1
}

trait Outer {
  trait Bar {
    def foo = 1
  }
}
