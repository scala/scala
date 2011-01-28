object A {
  def f1 = List(classOf[Int], classOf[String])
  def f2 = List(classOf[String], classOf[Int])
  def f3(x: Class[_ <: Int]) = x
  def f4(x: Class[_ <: String with Int]) = x
  def f5(x: Class[_ <: Int with String]) = x
}