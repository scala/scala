object Test {

  def main(args : Array[String]) : Unit = {
    recurse(Map(1->1, 2->2, 3->3, 4->4, 5->5, 6->6, 7->7))
    recurse(Set(1,2,3,4,5,6,7))
    println("done")
  }

  def recurse(map: collection.immutable.Map[Int, Int]): Unit = {
    if (!map.isEmpty) {
      val x = map.keys.head
      recurse(map - x)
    }
  }

  def recurse(set: collection.immutable.Set[Int]): Unit = {
    if (!set.isEmpty) {
      val x = set.toStream.head
      recurse(set - x)
    }
  }

}
