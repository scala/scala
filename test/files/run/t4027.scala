

import collection._


/** Sorted maps should have `filterKeys` and `mapValues` which return sorted maps.
 *  Mapping, filtering, etc. on these views should return sorted maps again.
 */
object Test extends App {

  val sortedmap = SortedMap(1 -> false, 2 -> true, 3 -> false, 4 -> true)
  println(sortedmap.filterKeys(_ % 2 == 0): SortedMap[Int, Boolean])
  println(sortedmap.mapValues(_ + "!"): SortedMap[Int, String])
  println(sortedmap.filterKeys(_ % 2 == 0).map(t => (t._1, t._2.toString.length)): SortedMap[Int, Int])
  println(sortedmap.mapValues(_ + "!").map(t => (t._1, t._2.toString.length)): SortedMap[Int, Int])
  println(sortedmap.filterKeys(_ % 2 == 0).filter(t => t._1 < 2): SortedMap[Int, Boolean])
  println(sortedmap.mapValues(_ + "!").filter(t => t._1 < 2): SortedMap[Int, String])

  val immsortedmap = immutable.SortedMap(1 -> false, 2 -> true, 3 -> false, 4 -> true)
  println(immsortedmap.filterKeys(_ % 2 == 0): immutable.SortedMap[Int, Boolean])
  println(immsortedmap.mapValues(_ + "!"): immutable.SortedMap[Int, String])
  println(immsortedmap.filterKeys(_ % 2 == 0).map(t => (t._1, t._2.toString.length)): immutable.SortedMap[Int, Int])
  println(immsortedmap.mapValues(_ + "!").map(t => (t._1, t._2.toString.length)): immutable.SortedMap[Int, Int])
  println(immsortedmap.filterKeys(_ % 2 == 0).filter(t => t._1 < 2): immutable.SortedMap[Int, Boolean])
  println(immsortedmap.mapValues(_ + "!").filter(t => t._1 < 2): immutable.SortedMap[Int, String])

}
