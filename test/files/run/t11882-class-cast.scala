object Test {
  def test[T <: AnyRef: reflect.ClassTag](t: T) = Array(t)
  def main(args: Array[String]): Unit = {
    // was: java.lang.ClassCastException: [Ljava.lang.Object; cannot be cast to [Ljava.lang.String;
    val x: Array[String] = test[String]("x")
    assert(x(0) == "x")
  }
}
