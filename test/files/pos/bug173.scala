object V with Executable {
  def foo[A](x: String) = new Object;
  def foo[A](x: Runnable) = new Object;
  val code = foo[Int]("test");
}
