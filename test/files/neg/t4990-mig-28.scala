object t4990_mig_28 extends App {
  collection.mutable.Buffer(1,2,3,4) - 3 //@migrated in 2.8
  List(1, 2, 3, 4).scanRight(0)(_ + _) //@migrated in 2.9
}

