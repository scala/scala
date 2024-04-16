//> using options -Werror
package p
package _root_
object Test {
  import _root_.scala.Option
  def f = Option(null)
}

// was:
// test/files/neg/t6217b.scala:5: error: object scala is not a member of package p._root_
