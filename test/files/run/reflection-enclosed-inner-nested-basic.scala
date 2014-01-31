import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror => cm}
import scala.reflect.{classTag, ClassTag}

class B {
  object BB {
    class B1 { override def toString = "B1"; def foo = 1 }
    private class B2 { override def toString = "B2"; def foo = 2 }
    object B3 { override def toString = "B3"; def foo = 3 }
    private object B4 { override def toString = "B4"; def foo = 4 }
    object B5 extends B1 { override def toString = "B5"; override def foo = 5 }
    private object B6 extends B2 { override def toString = "B6"; override def foo = 6 }
  }
}

object Test extends App {
  val outer1 = new B()
  val b = cm.moduleSymbol(classTag[outer1.BB.type].runtimeClass)
  println(b)
  println(b.info.decls.toList)

  def testMethodInvocation(instance: Any) = {
    val instanceMirror = cm.reflect(instance)
    val method = instanceMirror.symbol.info.decl(TermName("foo")).asMethod
    val methodMirror = instanceMirror.reflectMethod(method)
    println(methodMirror())
  }

  def testNestedClass(name: String) = {
    val sym = b.info.decl(TypeName(name)).asClass
    println(sym)
    val ctor = sym.info.decl(termNames.CONSTRUCTOR).asMethod
    val ctorMirror = cm.reflect(outer1.BB).reflectClass(sym).reflectConstructor(ctor)
    val instance = ctorMirror()
    println(instance)
    testMethodInvocation(instance)
  }

  testNestedClass("B1")
  testNestedClass("B2")

  def testNestedModule(name: String) = {
    val sym = b.info.decl(TermName(name)).asModule
    println(sym)
    val moduleMirror = cm.reflect(outer1.BB).reflectModule(sym)
    val instance = moduleMirror.instance
    println(instance)
    testMethodInvocation(instance)
  }

  testNestedModule("B3")
  testNestedModule("B4")
  testNestedModule("B5")
  testNestedModule("B6")
}
