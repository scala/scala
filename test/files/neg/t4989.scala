class A0 {
  def print(): String
}

class A extends A0 {
  def print(): String = "A"
}

abstract class B extends A {
  def print() : String
}

class C extends B {
  override def print(): String = super.print() // should be an error
}

class D extends A {
  override def print(): String = super.print() // okay
}
