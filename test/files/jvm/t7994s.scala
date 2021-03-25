object Test {
  def main(args: Array[String]): Unit = {
    val o = new MyTest() {
      val i: MyTest = new MyTest() {}
    }
  }
}

class MyTest {
  println(this.getClass.getName)
  println(this.getClass.getDeclaringClass)
}