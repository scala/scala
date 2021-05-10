object Test extends p.J.C with p.J.I {
  def main(args: Array[String]): Unit = {
    println((this: p.J.I).m.trim)
  }
}
