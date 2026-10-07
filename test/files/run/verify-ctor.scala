class Foo(val str: String) {
  def this(arr: Array[Char]) = this({
    if (arr.length == 0) Test.quit(1)
    new String(arr)
  })
}

object Test {
  def quit(s: Int): Nothing = ???
  def main(args: Array[String]) = {
    val t = new Foo(Array('a', 'b', 'c'))
    println(t.str)
  }
}
