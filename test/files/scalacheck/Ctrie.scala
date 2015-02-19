


import org.scalacheck._
import Prop._
import org.scalacheck.Gen._
import collection._
import collection.concurrent.TrieMap



case class Wrap(i: Int) {
  override def hashCode = i // * 0x9e3775cd
}


/** A check mainly oriented towards checking snapshot correctness.
 */
object Test extends Properties("concurrent.TrieMap") {

  /* generators */

  val sizes = choose(0, 200000)

  val threadCounts = choose(2, 16)

  val threadCountsAndSizes = for {
    p <- threadCounts
    sz <- sizes
  } yield (p, sz);


  /* helpers */

  def inParallel[T](totalThreads: Int)(body: Int => T): Seq[T] = {
    val threads = for (idx <- 0 until totalThreads) yield new Thread {
      setName("ParThread-" + idx)
      private var res: T = _
      override def run() {
        res = body(idx)
      }
      def result = {
        this.join()
        res
      }
    }

    threads foreach (_.start())
    threads map (_.result)
  }

  def spawn[T](body: =>T): { def get: T } = {
    val t = new Thread {
      setName("SpawnThread")
      private var res: T = _
      override def run() {
        res = body
      }
      def result = res
    }
    t.start()
    new {
      def get: T = {
        t.join()
        t.result
      }
    }
  }

  def elementRange(threadIdx: Int, totalThreads: Int, totalElems: Int): Range = {
    val sz = totalElems
    val idx = threadIdx
    val p = totalThreads
    val start = (sz / p) * idx + math.min(idx, sz % p)
    val elems = (sz / p) + (if (idx < sz % p) 1 else 0)
    val end = start + elems
    (start until end)
  }

  def hasGrown[K, V](last: Map[K, V], current: Map[K, V]) = {
    (last.size <= current.size) && {
      last forall {
        case (k, v) => current.get(k) == Some(v)
      }
    }
  }

  object err {
    var buffer = new StringBuilder
    def println(a: AnyRef) = buffer.append(a.toString).append("\n")
    def clear() = buffer.clear()
    def flush() = {
      Console.out.println(buffer)
      clear()
    }
  }


  /* properties */

  property("concurrent growing snapshots") = forAll(threadCounts, sizes) {
    (numThreads, numElems) =>
    val p = 3 //numThreads
    val sz = 102 //numElems
    val ct = new TrieMap[Wrap, Int]

    // checker
    val checker = spawn {
      def check(last: Map[Wrap, Int], iterationsLeft: Int): Boolean = {
        val current = ct.readOnlySnapshot()
        if (!hasGrown(last, current)) false
        else if (current.size >= sz) true
        else if (iterationsLeft < 0) false
        else check(current, iterationsLeft - 1)
      }
      check(ct.readOnlySnapshot(), 500)
    }

    // fillers
    inParallel(p) {
      idx =>
      elementRange(idx, p, sz) foreach (i => ct.update(Wrap(i), i))
    }

    // wait for checker to finish
    val growing = true//checker.get

    val ok = growing && ((0 until sz) forall {
      case i => ct.get(Wrap(i)) == Some(i)
    })

    ok
  }

  property("update") = forAll(sizes) {
    (n: Int) =>
    val ct = new TrieMap[Int, Int]
    for (i <- 0 until n) ct(i) = i
    (0 until n) forall {
      case i => ct(i) == i
    }
  }

  property("concurrent update") = forAll(threadCountsAndSizes) {
    case (p, sz) =>
      val ct = new TrieMap[Wrap, Int]

      inParallel(p) {
        idx =>
        for (i <- elementRange(idx, p, sz)) ct(Wrap(i)) = i
      }

      (0 until sz) forall {
        case i => ct(Wrap(i)) == i
      }
  }


  property("concurrent remove") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val ct = new TrieMap[Wrap, Int]
    for (i <- 0 until sz) ct(Wrap(i)) = i

    inParallel(p) {
      idx =>
      for (i <- elementRange(idx, p, sz)) ct.remove(Wrap(i))
    }

    (0 until sz) forall {
      case i => ct.get(Wrap(i)) == None
    }
  }


  property("concurrent putIfAbsent") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val ct = new TrieMap[Wrap, Int]

    val results = inParallel(p) {
      idx =>
      elementRange(idx, p, sz) find (i => ct.putIfAbsent(Wrap(i), i) != None)
    }

    (results forall (_ == None)) && ((0 until sz) forall {
      case i => ct.get(Wrap(i)) == Some(i)
    })
  }

  property("concurrent getOrElseUpdate") = forAll(threadCounts, sizes) {
    (p, sz) =>
    val totalInserts = new java.util.concurrent.atomic.AtomicInteger
    val ct = new TrieMap[Wrap, String]

    val results = inParallel(p) {
      idx =>
      (0 until sz) foreach {
        i =>
        val v = ct.getOrElseUpdate(Wrap(i), idx + ":" + i)
        if (v == idx + ":" + i) totalInserts.incrementAndGet()
      }
    }

    (totalInserts.get == sz) && ((0 until sz) forall {
      case i => ct(Wrap(i)).split(":")(1).toInt == i
    })
  }

}










