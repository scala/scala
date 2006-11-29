object testSome {

  val x: Option[String] = Some("foo")

  val x1: Option[Int] = Some(3)

  val y: Option[String] = None

  val y1: Option[Int] = None


  Console.println(x.isEmpty) // x ne null

  Console.println(x1.isEmpty)

  Console.println(x.get) // x

  Console.println(x1.get)

  def f(x:String) = Some(x)
  x.flatMap(&f)
}
