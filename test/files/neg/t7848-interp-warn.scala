
package test

object Test {
  def f = {
    val foo = "bar"
    "An important $foo message!"
  }
  def g = {
    val foo = "bar"
    "A doubly important ${foo * 2} message!"
  }
}
