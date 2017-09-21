object Test extends App {
  def foo(): () => String = () => "hi there"
  val f: () => Any = foo
  assert(f.toString.take(13) == "Test$$$Lambda", f)
  assert(f() == "hi there")
}
