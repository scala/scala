class C {
  def foo[T: scala.reflect.ClassTag](xs: T*): Array[T] = ???
  foo()
}