object test {

  val x: Object { def toString(): String } = new Object {
    override def toString(): String = "1";
  }
}
