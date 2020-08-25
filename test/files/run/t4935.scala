// scalac: -opt:l:inline -opt-inline-from:**
object Test extends App {
  for (i <- 0 to 1) {
    val a = Foo
  }
}

object Foo {
  println("hello")
}
