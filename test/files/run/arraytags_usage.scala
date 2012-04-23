object Test extends App {
  def foo[T] = {
    class MyArrayTag extends ArrayTag[T] {
      def wrap: ArrayTag[Array[T]] = ???
      def newArray(len: Int): Array[T] = new Array[Int](len).asInstanceOf[Array[T]]
    }

    implicit val tag = new MyArrayTag()
    println(Array[T]().getClass)
  }

  foo[Int]
  foo[String]
  foo[Array[String]]
}