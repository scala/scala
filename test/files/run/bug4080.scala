import scala.collection.mutable.LinkedList

object Test {
  def main(args: Array[String]) {
    val ll = LinkedList(1, 2, 3)
    ll.insert(LinkedList(0))
    println(ll)
    val ll2 = LinkedList[Int]()
    try println(ll2.head)
    catch { case _ => () }
  }
}
