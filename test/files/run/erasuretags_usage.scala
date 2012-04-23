object Test extends App {
  def foo[T] = {
    class MyErasureTag(_erasure: Class[_]) extends ErasureTag[T] {
      def erasure: Class[T] = _erasure.asInstanceOf[Class[T]]
    }

    implicit val tag = new MyErasureTag(classOf[Int])
    println(typeTag[T])
    println(typeTag[T].tpe)
    println(typeTag[T].erasure)
  }
}