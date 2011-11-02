object Test {
  def f1 = scala.collection.mutable.ListBuffer(1 to 9: _*).slice(-5, -1)
  def f2 = scala.collection.mutable.ListBuffer(1 to 9: _*).readOnly.slice(-5, -1)
  def f3 = Vector(1 to 9: _*).slice(-5, -1)
  def f4 = Traversable(1 to 9: _*).slice(-5, -1)
  def f5 = (1 to 9).toArray.slice(-5, -1)
  def f6 = (1 to 9).toStream.slice(-5, -1)
  def f7 = (1 to 9).slice(-5, -1)
  
  def main(args: Array[String]): Unit = {
    List[Traversable[Int]](f1, f2, f3, f4, f5, f6, f7) foreach (x => assert(x.isEmpty, x))
  }
}
