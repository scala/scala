trait A[@specialized(Int) K, @specialized(Double) V] {
  def doStuff(k: K, v: V): Unit = sys.error("I am overridden, you cannot call me")
}

trait B[@specialized(Double) V] extends A[Int, V] {
  override def doStuff(k: Int, v: V): Unit = println("Hi - I'm calling doStuff in B")
}

object Test {
  def main(args: Array[String]): Unit = delegate(new B[Double]() {}, 1, 0.1)

  def delegate[@specialized(Int) K, @specialized(Double) V](a: A[K, V], k: K, v: V) {
    a.doStuff(k, v)
  }
}

