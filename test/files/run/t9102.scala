
object Test extends App {
  import reflect.runtime._, universe._

  class C { def f(i: Int, j: => Int) = i + j }

  class V(val v: Int) extends AnyVal { def doubled = 2 * v }
  class D { def f(i: Int, j: V) = i + j.doubled }

  class E(i: Int, j: V)

  locally {
    val ms = typeOf[C].member(TermName("f")).asMethod
    val im = currentMirror reflect (new C)
    val mm = im reflectMethod ms
    assert(mm(2,3) == 5)
  }
  locally {
    val ms = typeOf[D].member(TermName("f")).asMethod
    val im = currentMirror reflect (new D)
    val mm = im reflectMethod ms
    assert(mm(2, new V(3)) == 8)
  }
  locally {
    val ms = typeOf[E].typeSymbol.asClass.primaryConstructor
    val cm = currentMirror reflectClass typeOf[E].typeSymbol.asClass
    val mm = cm reflectConstructor ms.asMethod
    assert(mm(42, new V(7)).isInstanceOf[E])
  }
}

/* Session tests without special init code should reside in simple script files.
 * Also, provide filters such as for `(bound to C@74f7d1d2)`.

import scala.tools.partest.SessionTest

object Test extends SessionTest {
//Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_40).
  def session =
    s"""|Type in expressions to have them evaluated.
        |Type :help for more information.
        |
        |scala> import reflect.runtime._, universe._
        |import reflect.runtime._
        |import universe._
        |
        |scala> class C { def f(i: Int, j: => Int) = i + j }
        |defined class C
        |
        |scala> typeOf[C].member(TermName("f"))
        |res0: reflect.runtime.universe.Symbol = method f
        |
        |scala> .asMethod
        |res1: reflect.runtime.universe.MethodSymbol = method f
        |
        |scala> currentMirror reflect (new C)
        |res2: reflect.runtime.universe.InstanceMirror = instance mirror for C@74f7d1d2
        |
        |scala> res2 reflectMethod res1
        |res3: reflect.runtime.universe.MethodMirror = method mirror for def f(i: scala.Int,j: => scala.Int): scala.Int (bound to C@74f7d1d2)
        |
        |scala> res3(2,3)
        |res4: Any = 5
        |
        |scala> :quit"""
}
*/

/* was:
scala> res3(2,3)
java.lang.IllegalArgumentException
  at sun.reflect.NativeMethodAccessorImpl.invoke0(Native Method)
  at sun.reflect.NativeMethodAccessorImpl.invoke(NativeMethodAccessorImpl.java:62)
  at sun.reflect.DelegatingMethodAccessorImpl.invoke(DelegatingMethodAccessorImpl.java:43)
  at java.lang.reflect.Method.invoke(Method.java:497)
  at scala.reflect.runtime.JavaMirrors$JavaMirror$JavaMethodMirror.jinvokeraw(JavaMirrors.scala:335)
  at scala.reflect.runtime.JavaMirrors$JavaMirror$JavaMethodMirror.jinvoke(JavaMirrors.scala:339)
  at scala.reflect.runtime.JavaMirrors$JavaMirror$JavaTransformingMethodMirror.apply(JavaMirrors.scala:436)
  ... 33 elided
*/

