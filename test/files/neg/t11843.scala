
trait T {
  def f = "".$asInstanceOf[Symbol]
  def g = "".$asInstanceOf[Int]
  def k = 42.$asInstanceOf[String]
  def p = 42.$isInstanceOf[String]
}
