import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.{classTag, ClassTag}

class B {
  class B1 { override def toString = "B1"; def foo = 1 }
  private class B2 { override def toString = "B2"; def foo = 2 }
  object B3 { override def toString = "B3"; def foo = 3 }
  private object B4 { override def toString = "B4"; def foo = 4 }
  object B5 extends B1 { override def toString = "B5"; override def foo = 5 }
  private object B6 extends B2 { override def toString = "B6"; override def foo = 6 }
}

object Test extends App {
  val b = cm.classSymbol(classTag[B].runtimeClass)
  println(b)
  println(b.typeSignature.declarations.toList)

  def testMethodInvocation(instance: Any) = {
    val instanceMirror = cm.reflect(instance)
    val method = instanceMirror.symbol.typeSignature.declaration(newTermName("foo")).asMethod
    val methodMirror = instanceMirror.reflectMethod(method)
    println(methodMirror())
  }

  def testInnerClass(name: String) = {
    val sym = b.typeSignature.declaration(newTypeName(name)).asClass
    println(sym)
    val ctor = sym.typeSignature.declaration(newTermName("<init>")).asMethod
    val ctorMirror = cm.reflect(new B).reflectClass(sym).reflectConstructor(ctor)
    val instance = ctorMirror()
    println(instance)
    testMethodInvocation(instance)
  }

  testInnerClass("B1")
  testInnerClass("B2")

  def testInnerObject(name: String) = {
    val sym = b.typeSignature.declaration(newTermName(name)).asObject
    println(sym)
    val objectMirror = cm.reflect(new B).reflectObject(sym)
    val instance = objectMirror.instance
    println(instance)
    testMethodInvocation(instance)
  }

  testInnerObject("B3")
  testInnerObject("B4")
  testInnerObject("B5")
  testInnerObject("B6")
}
