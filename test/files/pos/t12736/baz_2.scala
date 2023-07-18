package bar
package baz

import foo._

object Bar {
  def test() = println(Foo)
}

/*
baz_2.scala:8: error: object Foo in package foo cannot be accessed as a member of package foo from object Bar in package baz
  def test() = println(Foo)
                       ^
1 error
*/
