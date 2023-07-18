object Test extends App {

  def f(x: AnyVal, f: Runnable) = 1
  def f(x: Double, f: Runnable) = 2

  val ret1 = f(3.0, () => println("work")) // 2  match Double
  println(ret1)

  val ret2 = f(3, () => println("work")) // 2   match Double
  println(ret2)


  val ret3 = f('a', () => println("work")) // 2   match Double, char ==> Int => Double
  println(ret3)

  val ret4 = f(false, () => println("work")) // 1  match AnyVal
  println(ret4)

  val ret5 = f((), () => println("work")) // 1  match AnyVal
  println(ret5)

}