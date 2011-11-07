/** This is a test to make sure distinct always
 *  returns the first of any duplicated element.
 */
object Test {
  val alphabet = 'a' to 'z' mkString ""
  val alphaList = 'a' to 'z' toList
  def shuffled = util.Random.shuffle(alphaList)
  
  def main(args: Array[String]): Unit = {
    val longList = alphaList ++ (1 to 9 flatMap (_ => shuffled))
    val result = longList.distinct mkString ""

    println(result)
  }
}
