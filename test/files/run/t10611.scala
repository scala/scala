object Test extends App {
  /* crashes at jvm with AssertionError */
  def q[T: reflect.ClassTag] = Array.empty[T].length.##

  /* crashes at runtime with NoSuchMethodError */
  this.##.##
}