object Test {
  def foo(f: Int => Int) = () ; foo { implicit x : Int => x + 1 }
  def bar(f: Int => Int) = () ; foo { x : Int => x + 1 }
}
