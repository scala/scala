// no idea, reassigned to Iulian
object Test {

  def main(args: Array[String]) {
    val fooz = new foo.foo2
    println(fooz)
  }

  object foo {
    class foo2 {
      override def toString = getClass.toString//.getSimpleName
    }
  }

}
