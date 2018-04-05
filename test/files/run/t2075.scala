object Test extends App {
  var tm = new scala.collection.immutable.TreeMap[Int,Int]
  for (i <- 0 to 100)
    tm = tm.updated(i, i)

  tm.keySet.filter(_ < 40)
}
