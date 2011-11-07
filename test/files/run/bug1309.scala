object Test {
  def f(ras: => RandomAccessSeq[Byte]): RandomAccessSeq[Byte] = ras
  
  def main(args: Array[String]): Unit = {
    f(new Array[Byte](0))
  }
}
