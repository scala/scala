
object Test extends App {
  println {
    (new JTest, new KTest, ((s: String) => s): JFun)
  }
}
