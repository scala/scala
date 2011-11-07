object ImplicitTest {
  implicit val i : Int = 10
  implicit def a(implicit i : Int) : Array[Byte] = null
  implicit def b[T](implicit i : Int) : Array[T] = null
  
  def fn[T](implicit x : T) = 0
  
  val x = fn[Array[Byte]]
}