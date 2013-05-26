trait Mutable[@specialized A] { def a: A; def a_=(a0: A): Unit }
trait NotSpecialized { }
class Arr[@specialized A](val arr: Array[A]) {
  def bippy(m: Mutable[A]) { m.a = arr(0) }
  def quux(m: Mutable[A] with NotSpecialized) { m.a = arr(0) }
}

object Test {
  def main(args: Array[String]): Unit = {
    def quuxae = classOf[Arr[_]].getMethods filter (_.getName contains "quux")
    println(quuxae.size) // expect 10, not 1
  }
}
