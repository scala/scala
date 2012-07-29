/** This package has A and B.
  */
package test {
  trait A { def hi = "hello" }
  trait B { def bye = "bye!" }
}

/** This package object extends A and B.
  */
package object test extends A with B {
  override def hi = "good morning!"
  override def bye = "good bye!"
  protected def thank = "thank you!"
}
