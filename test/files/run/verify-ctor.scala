class Foo(val str: String) {
  def this(arr: Array[Char]) = this({
    if (arr.length == 0) exit(1)
    new String(arr)
  })
}

object Test {
  def main(args: Array[String]) = {
    val t = new Foo(Array('a', 'b', 'c'))
    println(t.str)
  }
}
