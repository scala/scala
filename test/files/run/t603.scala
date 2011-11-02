object forceDelay {
  class Susp[+A](lazyValue: => A) extends Function0[A] {
    private var func: () => Any = () => lazyValue
    private var value: Any = null

    override def apply() = {
      if (func != null) {
        value = func().asInstanceOf[A]
        func = null
      }
      value.asInstanceOf[A]
    }

    override def toString() =
      if (func == null) "Susp(" + value + ")"
      else "Susp(?)"
  }

  def delay[A](value: => A) = new Susp[A](value)
  implicit def force[A](s: Susp[A]): A = s()
}

object Test {
  import forceDelay._
  
  def main(args: Array[String]) = {
    val s: Susp[Int] = delay { Console.println("evaluating..."); 3 }
    Console.println("s = " + s)
    Console.println("s() = " + s())
    Console.println("s = " + s)
    Console.println("2 + s = " + (2 + s))
  }
}
