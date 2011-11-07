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
    override def defaultSize = 40000
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
      val f = operators.mapper2
      while (it.hasNext) {
        val p = f(it.next)
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
  
  object FlatMap2 extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "flatmap2";
    def apply(sz: Int, p: Int, w: String) = new FlatMap2(sz, p, w)
  }
  
  class FlatMap2(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    override def repetitionsPerRun = 25
    def runseq = this.seqcoll.flatMap(operators.flatmapper)
    def runpar = this.parcoll.flatMap(operators.flatmapper)
    def companion = FlatMap2
  }  
    
  object HeavyMap extends IterableBenchCompanion {
    override def defaultSize = 5000
    override def comparisons = List()
    def benchName = "heavy-map";
    def apply(sz: Int, p: Int, w: String) = new HeavyMap(sz, p, w)
  }
  
  class HeavyMap(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    var result: Int = 0
    def comparisonMap = collection.Map()
    def runseq = {
      val r = this.seqcoll.asInstanceOf[collection.mutable.HashMap[K, V]].map(operators.heavymapper)
      result = r.size
    }
    def runpar = {
      result = this.parcoll.map(operators.heavymapper).size
    }
    def companion = HeavyMap
    override def repetitionsPerRun = 50
  }
  
  object Reduce2 extends IterableBenchCompanion {
    override def defaultSize = 50000
    override def comparisons = List()
    def benchName = "reduce2";
    def apply(sz: Int, p: Int, w: String) = new Reduce2(sz, p, w)
  }
  
  class Reduce2(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.mediumreducer)
    def runpar = this.parcoll.reduce(operators.mediumreducer)
    def companion = Reduce2
  }
  
  object Foreach extends IterableBenchCompanion {
    override def defaultSize = 50000
    override def comparisons = List()
    def benchName = "foreach";
    def apply(sz: Int, p: Int, w: String) = new Foreach(sz, p, w)
  }
  
  class Foreach(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.foreach(operators.foreachFun)
    def runpar = this.parcoll.pforeach(operators.foreachFun)
    def companion = Foreach
  }
  
}





object RefParHashTableBenches extends ParHashTableBenches[Dummy, Dummy] {
  
  type DPair = (Dummy, Dummy);
  
  object ForeachSet extends IterableBenchCompanion {
    override def defaultSize = 50000
    override def comparisons = List()
    def benchName = "foreach-set";
    def apply(sz: Int, p: Int, w: String) = new ForeachSet(sz, p, w)
  }
  
  class ForeachSet(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val array = new Array[Int](size)
    def comparisonMap = collection.Map()
    def runseq = for (p <- this.seqcoll) array(p._1.in) += 1
    def runpar = this.parcoll.pforeach { p => array(p._1.in) += 1 }
    def companion = ForeachSet
    
    override def onEnd {
      for (i <- 0 until array.length) {
        assert(array(i) == repetitionsPerRun * runs)
      }
    }
  }
  
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
    val foreachFun = (t: DPair) => {
      t
      ()
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
    val flatmapper = (p: DPair) => {
      for (i <- 0 until 20) yield p
    }
    override val mapper2 = (p: DPair) => {
      val a = 1 //heavy(p._1.in)
      (new Dummy(p._1.in * -2 + a), p._2)
    }
    val heavymapper = (p: DPair) => {
      var i = -2000
      var t = p._1.in
      while (i < 0) {
        t += (p._2.num - p._1.num) / 500
        p._1.num += p._2.num + t
        i += 1
      }
      (p._1, new Dummy(0))
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
    collection.parallel.tasksupport.environment = forkJoinPool
    phm
  }
  
}
