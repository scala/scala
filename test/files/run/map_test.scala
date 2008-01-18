import scala.collection.immutable.{ListMap, Map, TreeMap}

object Test extends Application {
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
    val map1 = myMap.update(42,"The answer")
    val map2 = map1.update(17,"A small random number")
    val map3 = map2.update(666,"A bigger random number")
    val map4 = map3.update(4711,"A big random number")
    map1 == myMap + Pair(42, "The answer")
    var i = 0
    var map = map4
    while(i < 43) {
      map = map.update(i,i.toString())
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
