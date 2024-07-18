//> using options -Werror -Xlint:deprecation
//
abstract class Foo {
  def bar {}
  def baz
  def boo(i: Int, l: Long)
  def boz(i: Int, l: Long) {}
  def this(i: Int) { this() } // Don't complain here! or maybe do complain
  def foz: Unit               // Don't complain here!
}
