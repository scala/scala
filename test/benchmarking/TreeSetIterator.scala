
object TreeSetIterator {

  def main(args: Array[String]): Unit = {
    val n = 500000
    JavaUtilTS.main(args)
    MutableTS.main(args)
    ImmutableTS.main(args)
  }
}

class Dummy(val a: Int) extends math.Ordered[Dummy] {
    def compare(other: Dummy) = this.a - other.a

    override def toString = a.toString
  }


object JavaUtilTS extends testing.Benchmark {
  val length = sys.props("length").toInt
  var data: Array[Dummy] = (0 until length) map { a => new Dummy(a) } toArray
  var t: java.util.TreeSet[Dummy] = null

  def run = {
    t = new java.util.TreeSet[Dummy]()
    data foreach { a => t add a }

    var i: Dummy = null
    var it = t.iterator
    while (it.hasNext) {
      i = it.next
    }
    i
  }
}

object MutableTS extends testing.Benchmark {
  val length = sys.props("length").toInt
  var data: Array[Dummy] = (0 until length) map { a => new Dummy(a) } toArray
  var t: collection.mutable.TreeSet[Dummy] = null

  def run = {
    t = collection.mutable.TreeSet[Dummy](data: _*)

    var i: Dummy = null
    var it = t.iterator
    while (it.hasNext) {
      i = it.next
    }
    i
  }
}

object ImmutableTS extends testing.Benchmark {
  val length = sys.props("length").toInt
  var data: Array[Dummy] = (0 until length) map { a => new Dummy(a) } toArray
  var t: collection.immutable.TreeSet[Dummy] = null

  def run = {
    t = collection.immutable.TreeSet[Dummy](data: _*)

    var i: Dummy = null
    var it = t.iterator
    while (it.hasNext) {
      i = it.next
    }
    i
  }
}
