/** 
 * Tries to load a symbol for the `Foo$class` using Scala reflection.  
 * Since trait implementation classes do not get pickling information
 * symbol for them should be created using fallback mechanism
 * that exposes Java reflection information dressed up in
 * a Scala symbol.
 */
object Test extends App with Outer {
  import scala.reflect.mirror

  assert(mirror.classToSymbol(manifest[Foo].erasure).typeSignature.declaration(mirror.newTermName("bar")).typeSignature ==
    mirror.classToSymbol(manifest[Bar].erasure).typeSignature.declaration(mirror.newTermName("foo")).typeSignature)

  val s1 = implClass(manifest[Foo].erasure)
  assert(s1 != mirror.NoSymbol)
  assert(s1.typeSignature != mirror.NoType)
  assert(s1.companionSymbol.typeSignature != mirror.NoType)
  assert(s1.companionSymbol.typeSignature.declaration(mirror.newTermName("bar")) != mirror.NoSymbol)
  val s2 = implClass(manifest[Bar].erasure)
  assert(s2 != mirror.NoSymbol)
  assert(s2.typeSignature != mirror.NoType)
  assert(s2.companionSymbol.typeSignature != mirror.NoType)
  assert(s2.companionSymbol.typeSignature.declaration(mirror.newTermName("foo")) != mirror.NoSymbol)
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
