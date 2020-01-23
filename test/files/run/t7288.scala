trait TestTrait[@specialized(Double) T] {
    var array: Array[T] = null
}

class MyClass extends TestTrait[Double] {
    array = new Array[Double](0)
}

object Test extends App {
  val tst = new MyClass
}
