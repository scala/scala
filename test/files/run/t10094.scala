trait T[@specialized(Int) S] {
  def initialValue: S
  var value: S = initialValue
}

final class C[@specialized(Int) S](val initialValue: S) extends T[S]

object Test {
  def main(args: Array[String]): Unit =
    println(new C("hi").initialValue)
}
