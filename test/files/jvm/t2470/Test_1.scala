object Test {
  class Foo {
    @Action(block = Task.Scope.ACTION)
    def foo = 0
  }

  def main(args: Array[String]) {
    val m = classOf[Foo].getDeclaredMethods().find(_.toString.contains("foo")).get
    println(m.getAnnotations().toList)
  }
}
