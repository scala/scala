/**
 *  Testing scaladoc implicits elimination
 */
package scala.test.scaladoc.implicits.elimination {

  import language.implicitConversions // according to SIP18

  /** No conversion, as B doesn't bring any member */
  class A
  class B { class C; trait V; type T; }
  object A {
  	implicit def toB(a: A): B = null
  }
}
