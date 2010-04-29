object MyClass {
  val duplicate: Int = 10
}

class MyClass {
  private val duplicate = MyClass.duplicate
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new MyClass
    ()
  }
}
