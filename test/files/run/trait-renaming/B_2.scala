object Test {
  def main(args: Array[String]): Unit = {
    (new bippy.A).g.getDeclaredMethods.map(_.toString).sorted foreach println
  }
}
