
package test

object Test {
  def bar = "bar"
  def f = {
    val foo = "bar"
    "An important $foo message!"
  }
  def g = {
    val foo = "bar"
    "A doubly important ${foo * 2} message!"
  }
  def h = s"Try using '$$bar' instead."  // no warn
  def i = s"Try using '${ "$bar" }' instead."  // was: no warn on space test
  def j = s"Try using '${ "something like $bar" }' instead."  // warn
  def k = f"Try using '$bar' instead."  // no warn on other std interps
}
