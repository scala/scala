// martin 1-3-2002: it seems there is a problem with the way Serializable is loaded.
object test {

  def f() = "hello".concat("world");

}
// #1000
object A {
  println("""This a "raw" string ending with a "double quote"""")
}
