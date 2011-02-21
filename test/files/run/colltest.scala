import collection.mutable._
class TestSet(s0: Set[Int], s1: Set[Int]) {
  val Iterations = 10
  val Range = 100000
  val testEachStep = false
  val Threshold = 20000
  val r = new java.util.Random(12345)
  def test(s: Set[Int], n: Int): Any = {
    val v = n >> 3
    n & 7 match {
      case 0 | 1 | 2 => s contains v
      case 3         => s += v
      case 4         => s -= v
      case 5         => if (s.size > Threshold) s -= v else s += v
      case 6         => s += v
      case 7         => s.size
    }
  }
  def explain(n: Int, s: Set[Int]): String = n & 7 match {
    case 0 | 1 | 2 => "contains"
    case 3         => "add"
    case 4         => "remove"
    case 5         => if (s.size > Threshold) "remove" else "add"
    case 6         => "add"
    case 7         => "size"
  }
  def checkSubSet(pre: String, s0: Set[Int], s1: Set[Int]) {
    for (e <- s0.iterator)
      if (!(s1 contains e)) {
        assert(false, pre+" element: "+e+"\n S0 = "+s0+"\n S1 = "+s1)
      }
  }
  for (i <- 0 until Iterations) {
    val n = r.nextInt(Range)
    val res0 = test(s0, n)
    val res1 = test(s1, n)
    //Console.println("operation = "+explain(n, s0)+", value ="+(n >> 3)+", result0 = "+res0)
    if (testEachStep) {
      checkSubSet("superfluous", s0, s1)
      checkSubSet("missing", s1, s0)
    }
    if (res0 != res1)
      assert(false, "DIFFERENCE , operation = "+explain(n, s0)+", value ="+(n >> 3)+
             ", result0 = "+res0+", result1 = "+res1)
  }
  Console.println("succeeded for "+Iterations+" iterations.")
}
object Test extends App {
  def t3954 {
    import scala.collection.mutable
    import scala.collection.immutable
    val result = new mutable.ImmutableSetAdaptor(immutable.ListSet.empty[Int])
    println(result.add(1))
    println(result.add(1))
    val result2 = new mutable.HashSet[Int]
    println(result2.add(1))
    println(result2.add(1))
    val result3 = new java.util.HashSet[Int]()
    println(result3.add(1))
    println(result3.add(1))
  }
  t3954

  new TestSet(HashSet.empty, new scala.collection.mutable.LinkedHashSet)
}
