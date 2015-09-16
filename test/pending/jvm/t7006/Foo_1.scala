class Foo_1 {
  def foo {
    try {
      val x = 3 // this will be optimized away, leaving a useless jump only block
    } finally {
      print("hello")
    }
    while(true){} // ensure infinite loop doesn't break the algorithm
  }
}
