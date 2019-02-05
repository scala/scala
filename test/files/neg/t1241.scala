object test extends App {
  // more..
  type T = { def hello(): Unit }
  //val x4 = new AnyRef { def hello() { println("4") } } // ok!
  val x4 = new T { def hello(): Unit = { println("4") } }        // error!
  x4.hello()
  // more..
}
