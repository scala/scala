// scalac: -Xsource:3.0 -Xlint -Werror
//
object Foo {
  List(1,2,3).toSet()

  class A[T](val x: T)
  new A

  import java.text.SimpleDateFormat
  val sdf = new SimpleDateFormat("yyyyMMdd-HH0000")
  sdf.format()

  (42, 27) :: Nil     // nowarn
  Nil.::(42, 27)      // yeswarn
}
