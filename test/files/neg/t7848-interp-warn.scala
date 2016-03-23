
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
    "An important $foo message!"                              // warn on ident in scope
  }
  def g = {
    val foo = "bar"
    "A doubly important ${foo * 2} message!"                  // warn on some expr, see below
  }
  def h = s"Try using '$$bar' instead."                       // no warn
  def i = s"Try using '${ "$bar" }' instead."                 // was: no warn on space test
  def j = s"Try using '${ "something like $bar" }' instead."  // warn
  def k = f"Try using '$bar' instead."                        // no warn on other std interps
  def p = "Template ${} {}"                                   // no warn on unlikely or empty expressions
  def q = "${}$bar"                                           // disables subsequent checks! (a feature)
  def r = "${}${bar}"                                         // disables subsequent checks! (a feature)

  def v = "${baz}${bar}"                                      // warn on second expr
  def w = "${ op_* }"                                         // warn, only cheap ident parsing
  def x = "${ bar }"                                          // warn, a cheap ident in scope
  def y = "${ baz }"                                          // no warn, cheap ident not in scope
  def z = "${ baz * 3}"                                       // warn, no expr parsing
}
