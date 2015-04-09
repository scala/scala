
trait X {
  List(1, 2, 3).toSet.subsets.map(_.toList)     // ok now

  List(1, 2, 3).toSet.subsets().map(_.toList)   // now also
  List(1, 2, 3).toSet.subsets(2).map(_.toList)  // still ok
}
