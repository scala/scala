class A {
  def f = new Runnable {
    def run(): Unit = List(1,2).foreach(println)
  }
}
