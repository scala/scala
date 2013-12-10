object Foo {
  List(1,2,3).toSet()

  class A[T](val x: T)
  new A

  import java.text.SimpleDateFormat
  val sdf = new SimpleDateFormat("yyyyMMdd-HH0000")
  sdf.format()
}
