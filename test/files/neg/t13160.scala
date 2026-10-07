//> using options -deprecation -Werror

package p

import scala.language.implicitConversions

trait T {
  object i {
    class C
    private[p] object C {
      implicit def c2i(a: C): Int = 42
    }
  }
  object j {
    class D
    private[j] object D {
      implicit def d2i(a: D): Int = 42
    }
  }
  class E
  protected object E {
    implicit def e2i(a: E): Int = 42
  }
}

object Test extends T {
  def t1: Int = {
    val c = new i.C()
    c // ok
  }
  def t2: Int = {
    val d = new j.D()
    d // not
  }
  def t3: Int = {
    val e = new E()
    e // ok
  }
  def t4: Int = {
    val t = new T {}
    val e = new t.E()
    e // not
  }
}
