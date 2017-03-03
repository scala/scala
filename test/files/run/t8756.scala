trait Bippy[A]

class Test {
  type T1 = Long
  type T2 = Long { type Tag = Nothing }

  def f1(t: T1): Bippy[Object] = ???
  def f2(t: T2): Bippy[Object] = ???
  def g1(t: T1): Int           = ???
  def g2(t: T2): Int           = ???
  def h1(t: T1): Object        = ???
  def h2(t: T2): Object        = ???
  def i1(t: Bippy[T1]): Bippy[T1] = ???
  def i2(t: Bippy[T2]): Bippy[T2] = ???

}

object Test {
  def main(args: Array[String]) {
    println(classOf[Test].getDeclaredMethods.map(_.toGenericString).toList.sorted.mkString("\n"))
  }
}
