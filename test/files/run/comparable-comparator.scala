
object Test {
  import java.util.Comparator
  
  class C1(val s: String) extends Comparable[C1] {
    def compareTo(other: C1) = s compareTo other.s
    override def toString = s
  }
  class C2(val s: String) {
    def compareTo(other: C2) = s compareTo other.s
    override def toString = s
  }
  
  implicit val cmp: Comparator[C2] = new Comparator[C2] {
    def compare(p1: C2, p2: C2) = p2.s compareTo p1.s
  }

  val strs = "zip foo bar baz aggle bing bong" split ' ' toList
  val c1s = strs map (x => new C1(x))
  val c2s = strs map (x => new C2(x))
  
  val sorted1 = c1s.sorted map (_.s)
  val sorted2 = c2s.sorted map (_.s)
  
  def main(args: Array[String]): Unit = {
    assert(sorted1 == sorted2.reverse)
  }
}
