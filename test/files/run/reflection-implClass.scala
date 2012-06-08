/**
 * Tries to load a symbol for the `Foo$class` using Scala reflection.
 * Since trait implementation classes do not get pickling information
 * symbol for them should be created using fallback mechanism
 * that exposes Java reflection information dressed up in
 * a Scala symbol.
 */
object Test extends App with Outer {
  import scala.reflect.{ClassTag, classTag}
  import scala.reflect.runtime.universe._
  import scala.reflect.runtime.{currentMirror => cm}

  assert(cm.classSymbol(classTag[Foo].runtimeClass).typeSignature.declaration(newTermName("bar")).typeSignature ==
    cm.classSymbol(classTag[Bar].runtimeClass).typeSignature.declaration(newTermName("foo")).typeSignature)

  val s1 = implClass(classTag[Foo].runtimeClass)
  assert(s1 != NoSymbol)
  assert(s1.typeSignature != NoType)
  assert(s1.companionSymbol.typeSignature != NoType)
  assert(s1.companionSymbol.typeSignature.declaration(newTermName("bar")) != NoSymbol)
  val s2 = implClass(classTag[Bar].runtimeClass)
  assert(s2 != NoSymbol)
  assert(s2.typeSignature != NoType)
  assert(s2.companionSymbol.typeSignature != NoType)
  assert(s2.companionSymbol.typeSignature.declaration(newTermName("foo")) != NoSymbol)
  def implClass(clazz: Class[_]) = {
    val implClass = Class.forName(clazz.getName + "$class")
    cm.classSymbol(implClass)
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
