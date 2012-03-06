object Test {
  def main(args: Array[String]) {
    val f = () => return try { 1 } catch { case _ => 0 }
    f()
  }
}
