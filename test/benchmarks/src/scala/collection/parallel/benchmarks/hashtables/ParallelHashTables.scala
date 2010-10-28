package scala.collection.parallel.benchmarks.hashtables




import scala.collection.parallel.benchmarks.generic.StandardParIterableBenches
import scala.collection.parallel.benchmarks.generic.Dummy
import scala.collection.parallel.benchmarks.generic.Operators
import scala.collection.parallel.mutable.ParHashMap





trait ParHashTableBenches[K, V] extends StandardParIterableBenches[(K, V), ParHashMap[K, V]] {

  def nameOfCollection = "mutable.ParHashMap"
  def comparisonMap = collection.mutable.Map()
  val forkJoinPool = new scala.concurrent.forkjoin.ForkJoinPool

  object Map2 extends IterableBenchCompanion {
    override def defaultSize = 5000
    override def comparisons = List("jhashtable")
    def benchName = "map2";
    def apply(sz: Int, p: Int, w: String) = new Map2(sz, p, w)
  }

  class Map2(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    var result: Int = 0
    def comparisonMap = collection.Map("jhashtable" -> runjhashtable _)
    def runseq = {
      val r = this.seqcoll.asInstanceOf[collection.mutable.HashMap[K, V]].map(operators.mapper2)
      result = r.size
    }
    def runpar = {
      result = this.parcoll.map(operators.mapper2).size
    }
    def runjhashtable = {
      val jumap = new java.util.HashMap[K, V]()
      val it = this.seqcoll.iterator
      while (it.hasNext) {
        val p = it.next
        jumap.put(p._1, p._2)
      }
      result = jumap.size
    }
    override def reset = runWhat match {
      case "jhashtable" => this.seqcoll = createSequential(size, parallelism)
      case _ => super.reset
    }
    def companion = Map2
    override def repetitionsPerRun = 50
    override def printResults {
      println("Size of last result: " + result)
    }
  }

  object Reduce2 extends IterableBenchCompanion {
    override def defaultSize = 50000
    override def comparisons = List()
    def benchName = "reduce2";
    def apply(sz: Int, p: Int, w: String) = new Reduce2(sz, p, w)
  }

  class Reduce2(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    private var ht: collection.mutable.HashMap[K, V] = _
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.mediumreducer)
    def runpar = this.parcoll.reduce(operators.mediumreducer)
    override def reset = runWhat match {
      case _ => super.reset
    }
    def companion = Reduce2
  }

}





object RefParHashTableBenches extends ParHashTableBenches[Dummy, Dummy] {

  type DPair = (Dummy, Dummy)

  object operators extends Operators[DPair] {
    def gcd(a: Int, b: Int): Int = {
      val result = if (b == 0) a else {
        gcd(b, a - b * (a / b))
      }
      result + 1000
    }
    def heavy(a: Int): Int = {
      var i = 0
      var sum = a
      while (i < 3000) {
        i += 1
        sum += a + i
      }
      sum
    }
    val reducer = (x: DPair, y: DPair) => {
      //y._2.num = x._2.in + y._2.in
      y
    }
    val mediumreducer = (x: DPair, y: DPair) => {
      y._2.num = gcd(x._2.in, y._2.in)
      y
    }
    val filterer = (p: DPair) => {
      p._1.num % 2 == 0
    }
    val mapper = (p: DPair) => {
      val a = p._1
      a.num = a.in % 2
      (a, p._2)
    }
    override val mapper2 = (p: DPair) => {
      val a = 1 //heavy(p._1.in)
      (new Dummy(p._1.in * -2 + a), p._2)
    }
    val heavymapper = (p: DPair) => {
      val a = p._1
      var i = -100
      while (i < 0) {
        if (a.in < i) a.num += 1
        i += 1
      }
      (a, p._2)
    }
    val taker = (p: DPair) => true
    val eachFun: DPair => Unit = { dp =>
      dp._1.dummy
    }
  }

  def createSequential(sz: Int, p: Int) = {
    val ht = new collection.mutable.HashMap[Dummy, Dummy]
    for (i <- 0 until sz) ht += ((new Dummy(i), new Dummy(i)))
    ht
  }

  def createParallel(sz: Int, p: Int) = {
    val phm = new ParHashMap[Dummy, Dummy]
    for (i <- 0 until sz) phm += ((new Dummy(i), new Dummy(i)))
    forkJoinPool.setParallelism(p)
    phm.environment = forkJoinPool
    phm
  }

}
