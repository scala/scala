object Test {
  object C {
    class B1
    private class B2
    object B3
    private object B4
    object B5 extends B1
    private object B6 extends B2

    val classes1 = this.getClass.getDeclaredClasses
    val classes2 = C.getClass   .getDeclaredClasses
    val classes3 = getClass     .getDeclaredClasses
  }

  // sortBy(_.getName) introduces additional classes which we don't want to see in C,
  // so we call sortBy outside of C.
  object TestHelper {
    val valuesTry1 = C.classes1.sortBy(_.getName)
    val valuesTry2 = C.classes2.sortBy(_.getName)
    val valuesTry3 = C.classes3.sortBy(_.getName)
  }

  def main(args: Array[String]) {
    println("Try 1: (" + TestHelper.valuesTry1.length + " classes)")
    TestHelper.valuesTry1.foreach(println)
    println("Try 2: (" + TestHelper.valuesTry2.length + " classes)")
    TestHelper.valuesTry2.foreach(println)
    println("Try 3: (" + TestHelper.valuesTry3.length + " classes)")
    TestHelper.valuesTry3.foreach(println)
  }


}

