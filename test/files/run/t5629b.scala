object Test extends App {

  trait MyPF[@specialized(Int) -A] extends (A => Unit) {
    def isDefinedAt(x: A): Boolean
    def applyOrElse[A1 <: A](x: A1, default: A1 => Unit): Unit = {
      println("MyPF.applyOrElse entered...")
      if (isDefinedAt(x)) apply(x) else default(x)
    }
  }

  trait MySmartPF[@specialized(Int) -A] extends MyPF[A] {
    def apply(x: A): Unit = {
      println("MySmartPF.apply entered...")
      applyOrElse(x, { default: Any => throw new MatchError(default) })
    }
  }

  type T = Int
  //type T = Any

  def newPF(test: T): MyPF[T] = new MySmartPF[T] {
    def isDefinedAt(x: T): Boolean = x != test
    override def applyOrElse[A1 <: T](x: A1, default: A1 => Unit): Unit = {
      println("newPF.applyOrElse entered...")
      if (x != test) { println("ok"); () } else { println("default"); default(x) }
    }
  }

  val pf = newPF(1)
  println("=== pf(1):")
  try { pf(1) } catch { case x: Throwable => println(x) }
  println("=== pf(42):")
  pf(42)
  println("=== done")

}
