// Compiles
package a {
  case class A[T]()
}

// Compiles
package b.scala {
  class B[T]
}

// Doesn't compile: type Nothing is not a member of c.scala
package c.scala {
  case class C[T]()
}

// Doesn't compile: type Nothing is not a member of d.scala 
package d.scala.d {
  case class D[T]()
}

// Doesn't compile: type Any is not a member of e.scala 
package e.scala {
  case class E[T >: Nothing]()
}

// Compiles
package f.scala {
  case class F[T >: Nothing <: Any]()
}

// Doesn't compile: type <byname> is not a member of package h.scala
package h.scala {
  case class H(s: String)(t: =>String)
}
