/**
 *  Testing scaladoc implicit scopes - looking for implicits in the right places
 */
package scala.test.scaladoc.implicits.scopes
import language.implicitConversions // according to SIP18

// TEST1 - In package object
package object test1 {
  implicit def toB(a: A): B = null
}
package test1 {
  class A
  class B { def b = "" }
}

// TEST2 - In enclosing package - doesn't seem to work even in scalac
package object test2 {
  import classes._
  implicit def toB(a: A): B = null
}
package test2 {
  package classes {
    class A
    class B { def b = "" }
    object test { (new A).b }
  }
}

// TEST3 - In companion object
package test3 {
  class A
  object A { implicit def toB(a: A): B = null }
  class B { def b = "" }
}

// TEST4 - Nested type's companion object
package test4 {
  class U[V]
  class S
  object S { implicit def toB(a: A): B = null }
  class A extends U[S]
  class B { def b = "" }
}

// TEST5 - In scope
package test5 {
  object scope {
    class A
    class B { def b = "" }
    implicit def toB(a: A): B = null
  }
}
