

import collection._

// Sorted maps have `filterKeys` and `mapValues` which return MapView.
// Calling a transformation (map/filter) returns a View.
object Test extends App {
  val sortedmap = SortedMap(1 -> false, 2 -> true, 3 -> false, 4 -> true)
  println((sortedmap.view.filterKeys(_ % 2 == 0): MapView[Int, Boolean]).toMap)
  println((sortedmap.view.mapValues(_.toString + "!"): MapView[Int, String]).toMap)
  println((sortedmap.view.filterKeys(_ % 2 == 0).map(t => (t._1, t._2.toString.length)): View[(Int, Int)]).toMap)
  println((sortedmap.view.mapValues(_.toString + "!").map(t => (t._1, t._2.toString.length)): View[(Int, Int)]).toMap)
  println((sortedmap.view.filterKeys(_ % 2 == 0).filter(t => t._1 < 2): View[(Int, Boolean)]).toMap)
  println((sortedmap.view.mapValues(_.toString + "!").filter(t => t._1 < 2): View[(Int, String)]).toMap)

  val immsortedmap = immutable.SortedMap(1 -> false, 2 -> true, 3 -> false, 4 -> true)
  println((immsortedmap.view.filterKeys(_ % 2 == 0): MapView[Int, Boolean]).toMap)
  println((immsortedmap.view.mapValues(_.toString + "!"): MapView[Int, String]).toMap)
  println((immsortedmap.view.filterKeys(_ % 2 == 0).map(t => (t._1, t._2.toString.length)): View[(Int, Int)]).toMap)
  println((immsortedmap.view.mapValues(_.toString + "!").map(t => (t._1, t._2.toString.length)): View[(Int, Int)]).toMap)
  println((immsortedmap.view.filterKeys(_ % 2 == 0).filter(t => t._1 < 2): View[(Int, Boolean)]).toMap)
  println((immsortedmap.view.mapValues(_.toString + "!").filter(t => t._1 < 2): View[(Int, String)]).toMap)
}
