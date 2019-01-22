object Test extends App {
  val result = new Test_1().doubler().asInstanceOf[T[Int]].t(1)
  assert(result == 2)
}
