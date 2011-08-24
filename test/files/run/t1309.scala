object Test {
  def f(ras: => IndexedSeq[Byte]): IndexedSeq[Byte] = ras

  def main(args: Array[String]): Unit = {
    f(new Array[Byte](0))
  }
}
