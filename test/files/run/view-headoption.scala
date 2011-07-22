object Test {
  val failer    = () => { println("fail") ; None }
  val succeeder = () => { println("success") ; Some(5) }
  val fs = List(failer, succeeder, failer, failer, succeeder, failer, failer, failer, succeeder)

  def f0 = fs.view flatMap (f => f())
  def f1 = f0.headOption
  def f2 = f0.head
  def f3 = f0.lastOption
  def f4 = f0.last

  def main(args: Array[String]): Unit = {
    println("f1: " + f1)
    println("f2: " + f2)
    println("f3: " + f3)
    println("f4: " + f4)
  }
}
