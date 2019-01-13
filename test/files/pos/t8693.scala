class A {
  def print(): Unit = {  }
}

trait B extends A {
  def print(): Unit
}

trait T3 extends B {
  override def print(): Unit = { super[B].print }
}