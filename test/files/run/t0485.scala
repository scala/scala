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
  test(new HashMap[Int, Int])
  test(new IdentityHashMap[Int, Int])
  test(new LinkedHashMap[Int, Int])
  test(new TreeMap[Int, Int])
  test(new WeakHashMap[Int, Int])
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
