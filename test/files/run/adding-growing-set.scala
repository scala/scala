/** This will run a loooong time if Set's builder copies a
 *  complete new Set for every element.
 */
object Test {
  def main(args: Array[String]): Unit = {
    val a = new Array[Long](1000000)
    (1 to 10000) foreach (i => a(i) = i)
    val s = collection.mutable.Set(a: _*)
    assert(s.sum > 0)
  }
}
