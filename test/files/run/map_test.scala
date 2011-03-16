import scala.collection.immutable.{ListMap, Map, TreeMap}

object Test extends App {
  test1()
  test2()
  println("OK")

  def test1() {
    val myMap: TreeMap[Int, String] = new TreeMap
    test_map(myMap)
  }

  def test2() {
    val myMap: ListMap[Int, String] = new ListMap
    test_map(myMap)
  }

  def test_map(myMap: Map[Int, String]) {
    val map1 = myMap.updated(42,"The answer")
    val map2 = map1.updated(17,"A small random number")
    val map3 = map2.updated(666,"A bigger random number")
    val map4 = map3.updated(4711,"A big random number")
    map1 == myMap + Pair(42, "The answer")
    var i = 0
    var map = map4
    while(i < 43) {
      map = map.updated(i,i.toString())
      i += 1
    }
    i = 0
    while(i < 4712) {
      if (map.isDefinedAt(i))
        print(i + "->" + map(i) + " ");
      i += 1
    }
    println("")
  }
}
