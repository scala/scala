
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
