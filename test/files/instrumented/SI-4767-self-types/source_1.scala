package instrumented

trait A {
  this: A with B =>

  // check from A
  debug("hello from A")
}

trait B {
  this: A with B =>

  @inline final def debug(msg: String) = println(msg)

  // check from B
  debug("hello from B")
}
