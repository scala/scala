//> using options -Werror -Wunused:params

class C {
  def answer: 42 = 42
  object X
  def g0(x: Int) = ???
  def f0(x: Int) = ()
  def f1(x: Int) = throw new RuntimeException
  def f2(x: Int) = 42
  def f3(x: Int): Option[Int] = None
  def f4(x: Int) = classOf[Int]
  def f5(x: Int) = answer + 27
  def f6(x: Int) = X
  def f7(x: Int) = Y
  def f8(x: Int): List[C] = Nil
  //def z0(x: Int) = X.toString
  //def z1(x: Int) = (42: Int)
}
object Y
