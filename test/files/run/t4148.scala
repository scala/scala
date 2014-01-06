object Test {
  val x1 = try { "aaa".asInstanceOf[Int] } catch { case _: Throwable => "cce1" }
  val x2 = try { (5: Any).asInstanceOf[Int] } catch { case _: Throwable => "cce2" }
  val x3 = try { (new java.lang.Short(100.toShort).asInstanceOf[Int]) } catch { case _: Throwable => "cce3" }

  def main(args: Array[String]): Unit = {
    List(x1, x2, x3) foreach println
  }
}
