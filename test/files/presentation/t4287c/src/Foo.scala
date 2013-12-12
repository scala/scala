class A(a: Int = A/*#*/.a/*#*/)

object A {
  val a = 2
}

class B extends A {
 def this(a) = this()
}