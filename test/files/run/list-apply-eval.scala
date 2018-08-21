object Test {
  var counter = 0
  def next = {
    counter += 1
    counter.toString
  }
  def main(args: Array[String]) {
    //List.apply is subject to an optimisation in cleanup
    //ensure that the arguments are evaluated in the currect order
    // Rewritten to:
    //      val myList: List = new collection.immutable.::(Test.this.next(), new collection.immutable.::(Test.this.next(), new collection.immutable.::(Test.this.next(), scala.collection.immutable.Nil)));
    val myList = List(next, next, next)
    assert(myList == List("1", "2", "3"), myList)
  }
}
