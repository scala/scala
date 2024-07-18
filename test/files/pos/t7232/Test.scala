//> using options -Werror
object Test {
  import pack._
  Foo.okay().size()
  Foo.wrong().size()
}
