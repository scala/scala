// a.scala
// Fri Jan 20 12:22:51 PST 2012

class A(xs: Int*) { def getXs = xs }

class B extends A { override def getXs = Nil }
