package tastytest

object TestReflection {
  def test1 = assert(new Reflection().fieldAtIndex(1) == "Hello") // test reading `classOf` in the @throws annotation
}
