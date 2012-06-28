abstract class A0 {
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

trait T extends B {
  override def print(): String = super.print() // should be an error
}

class D extends A {
  override def print(): String = super.print() // okay
}


// it's okay do this when trait are in the mix, as the
// suitable super accessor methods are used.
object ConcreteMethodAndIntermediaryAreTraits {
  trait T1 {
    def print(): String = ""
  }

  trait T2 extends T1 {
    def print(): String
  }

  class C3 extends T2 {
    def print(): String = super.print() // okay
  }
}

object IntermediaryIsTrait {
  class T1 {
    def print(): String = ""
  }

  trait T2 extends T1 {
    def print(): String
  }

  class C3 extends T2 {
    override def print(): String = super.print() // okay
  }
}

object ConcreteMethodIsTrait {
  trait T1 {
    def print(): String = ""
  }

  abstract class T2 extends T1 {
    def print(): String
  }

  class C3 extends T2 {
    override def print(): String = super.print() // okay
  }
}
