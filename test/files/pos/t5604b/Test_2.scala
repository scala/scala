// sandbox/t5604/Test.scala
package t6504

object Test {
  def blerg1(a: Any): Any = if (foo) blerg1(0)
  def blerg2(a: Any): Any = if (t6504.foo) blerg2(0)
}
