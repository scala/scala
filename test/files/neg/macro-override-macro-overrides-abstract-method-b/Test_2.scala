object Test extends App {
  val c1 = new A with C {}
  val c2 = new C with A {}
  val c3 = new C with A { override def t(): Unit = macro Macro.t }
  val c4 = new C with A { override def t(): Unit = () }
}