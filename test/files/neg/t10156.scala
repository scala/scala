object t10156 {
  trait A
  def x(implicit a: A) = a
  val z = x _
}