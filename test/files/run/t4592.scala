object Test {
  def repeat[T](count: Int = 1, x: Boolean = true)(thunk: => T) : T = (0 until count).map(_ => thunk).last
  def repeat[T](thunk: => T) : T = repeat()(thunk)

  def main(args: Array[String]): Unit = {
    println(repeat(3.14))
    println(repeat(count=5)(3.14))
    println(repeat(count=5,x=false)(3.14))
  }
}
