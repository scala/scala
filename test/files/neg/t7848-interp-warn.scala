
package test

package pancake { }

object Test {
  type NonVal = Int

  def ok = "Don't warn on $nosymbol interpolated."

  def pass = "Don't warn on $pancake package names."

  def types = "Or $NonVal type symbols either."

  def bar = "bar"
  def f = {
    val foo = "bar"
    "An important $foo message!"
  }
  def g = {
    val foo = "bar"
    "A doubly important ${foo * 2} message!"
  }
  def h = s"Try using '$$bar' instead."                       // no warn
  def i = s"Try using '${ "$bar" }' instead."                 // was: no warn on space test
  def j = s"Try using '${ "something like $bar" }' instead."  // warn
  def k = f"Try using '$bar' instead."                        // no warn on other std interps
  def p = "Template ${} {}"                                   // no warn on unlikely or empty expressions
  def q = "${}$bar"                                           // disables subsequent checks!
}
