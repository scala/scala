class Ptr[T](var value: T) {
   def `unary_!` : T = value
   def `unary_!_=`(value: T): Unit = this.value = value
}

object Test extends App {
  def test = {
    val x = new Ptr(9)
    !x = 10
    !{ println("hi") ; x } = 11
    println(!x)
  }
  test
}
