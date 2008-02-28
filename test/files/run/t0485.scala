import scala.collection.jcl

object Test extends Application {
  testMap
  testSet
}

object testMap {
  def toString(m1: collection.Map[Int, Int]): String =
    m1.toList.sort((x, y) => x < y).mkString("{", ", ", "}")
  def test(m1: jcl.Map[Int, Int]) {
    try {
      m1.put(10, 20)
      val m2 = m1.clone()
      m1.put(20, 30)
      println("m1="+toString(m1))
      println("m2="+toString(m2))
      println("m1.size > m2.size is "+ (m1.size > m2.size))
      m1.remove((20, 30))
      println("m1 equals m2 is "+ (m1 equals m2))
      println()
    }
    catch {
      case e: Exception =>
        println(e); println()
    }
  }
  test(new jcl.HashMap[Int, Int])
  // Clone on IdentityHashMap of java-ibm-1.6 behaves differently than all others
  // Therefore, for now we will not perform this test on it.
  // test(new jcl.IdentityHashMap[Int, Int])
  test(new jcl.LinkedHashMap[Int, Int])
  test(new jcl.TreeMap[Int, Int])
  test(new jcl.WeakHashMap[Int, Int])
}

object testSet {
  def toString(s1: collection.Set[Int]): String =
    s1.toList.sort((x, y) => x < y).mkString("[", ", ", "]")
  def test(s1: jcl.Set[Int]) {
    s1.add(10)
    val s2 = s1.clone()
    s1.add(20)
    println("s1="+toString(s1))
    println("s2="+toString(s2))
    println("s1.size > s2 is "+ (s1.size > s2.size))
    s1.remove(20)
    println("s1 equals s2 is "+ (s1 equals s2))
    println()
  }
  test(new jcl.HashSet[Int])
  test(new jcl.LinkedHashSet[Int])
  test(new jcl.TreeSet[Int])
}
