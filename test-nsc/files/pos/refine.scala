object test {

  val x: Object { def t(): String } = new Object {
    def t(): String = "1";
  }
}
