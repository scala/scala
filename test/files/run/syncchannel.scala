object Test {
  def main(args: Array[String]) {
    val c = new scala.concurrent.SyncChannel[Int]
    scala.concurrent.ops.par({ c.write(42) }, { println(c.read) })
  }
}
