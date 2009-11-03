object Test {
	import scala.collection._
	((Map(1 -> "a", 2 -> "b"): collection.Map[Int, String]) map identity[(Int, String)]) : scala.collection.Map[Int,String]
	((SortedMap(1 -> "a", 2 -> "b"): collection.SortedMap[Int, String]) map identity[(Int, String)]): scala.collection.SortedMap[Int,String]
	((SortedSet(1, 2): collection.SortedSet[Int]) map identity[Int]): scala.collection.SortedSet[Int]
}