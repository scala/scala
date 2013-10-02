object Test {
  type Tag[X] = {type Tag = X}
  type TaggedArray[T] = Array[T] with Tag[Any]

  def method[T: scala.reflect.ClassTag](a: TaggedArray[T], value: T) {
    a.update(0, value)
    a foreach println
  }

  def main(args: Array[String]): Unit = {
    method(Array(1, 2).asInstanceOf[TaggedArray[Int]], 1)
  }
}
