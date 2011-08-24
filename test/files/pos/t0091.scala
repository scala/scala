class Bug {
  def main(args: Array[String]) = {
    var msg: String = null; // no bug if "null" instead of "_"
    val f:  PartialFunction[Any, Unit] = { case 42 => msg = "coucou" };
  }
}
