
object TreeSetIterator {

  def main(args: Array[String]): Unit = {
    val n = 500000
    new JavaUtilTS(n).main(args)
    new MutableTS(n).main(args)
    new ImmutableTS(n).main(args)
  }
}

class Dummy(val a: Int) extends math.Ordered[Dummy] {
    def compare(other: Dummy) = this.a - other.a

    override def toString = a.toString
  }


class JavaUtilTS(val length: Int) extends testing.Benchmark {
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

class MutableTS(val length: Int) extends testing.Benchmark {
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

class ImmutableTS(val length: Int) extends testing.Benchmark {
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
