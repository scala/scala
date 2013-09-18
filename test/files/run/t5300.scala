object Test {
  val pf: PartialFunction[Any, Unit] = { case _ => () }

  def main(args: Array[String]): Unit = {
    pf orElse pf
  }
}
