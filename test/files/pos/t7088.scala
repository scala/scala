object Example extends App {
  type Tag[X] = {type Tag = X}
  type TaggedArray[T] = Array[T] with Tag[Any]
 
  def method[T: reflect.ClassTag](a: TaggedArray[T], value: T) {a.update(0, value)}
 
  method(Array(1, 2).asInstanceOf[TaggedArray[Int]], 1)
}
