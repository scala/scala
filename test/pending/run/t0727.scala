object t0727 extends Application {
  val arr = Array("foo")
  println(arr.isInstanceOf[Array[String]])
  println(Array("foo").isInstanceOf[Array[String]])
}
