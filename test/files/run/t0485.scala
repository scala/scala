import scala.collection.jcl._

object Test extends Application {
  testMap
  testSet
}

object testMap {
  def test(m1: Map[Int, Int]) {
    try {
      m1.put(10, 20)
      val m2 = m1.clone()
      m1.put(20, 30)
      println("m1="+m1)
      println("m2="+m2)
      println("m1 == m2 is "+ (m1 == m2))
      println()
    }
    catch {
      case e: Exception =>
        println(e); println()
    }
  }
  def test1(m1: Map[Int, Int]) {
    try {
      m1.put(10, 20)
      val m2 = m1.clone()
      m1.put(20, 30)
      println("m1.size > m2.size is "+ (m1.size > m2.size))
      println()
    }
    catch {
      case e: Exception =>
        println(e); println()
    }
  }
  test(new HashMap[Int, Int])
  test(new LinkedHashMap[Int, Int])
  // NB. class IdentityHashMap makes no guarantees as to the order
  // of the map; in particular, it does not guarantee that the order
  // will remain constant over time (see Java API docs).
  //test1(new IdentityHashMap[Int, Int])
  test(new TreeMap[Int, Int])
  test(new WeakHashMap[Int, Int])
  test1(new IdentityHashMap[Int, Int])
}

object testSet {
  def test(m1: Set[Int]) {
    m1.add(10)
    val m2 = m1.clone()
    m1.add(20)
    println("m1="+m1)
    println("m2="+m2)
    println("m1 == m2 is "+ (m1 == m2))
    println()
  }
  test(new HashSet[Int])
  test(new LinkedHashSet[Int])
  test(new TreeSet[Int])
}
