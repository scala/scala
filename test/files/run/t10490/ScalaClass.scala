class Foo {
  class Bar {
    override def toString: String = "Foo$Bar was instantiated!"
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    // JavaClass is the user of the Scala defined classes
    println(JavaClass.bar)
    //println(JavaClass.baz)
  }
}