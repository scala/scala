package scala.collection.parallel.benchmarks.hashtables




import scala.collection.parallel.benchmarks.generic.StandardParIterableBenches
import scala.collection.parallel.benchmarks.generic.Dummy
import scala.collection.parallel.benchmarks.generic.DummyOperators
import scala.collection.parallel.mutable.ParHashSet





trait ParHashTableSetBenches[T] extends StandardParIterableBenches[T, ParHashSet[T]] {
  
  def nameOfCollection = "mutable.ParHashSet"
  def comparisonMap = collection.mutable.Set()
  val forkJoinPool = new scala.concurrent.forkjoin.ForkJoinPool
  
  object Map2 extends IterableBenchCompanion {
    override def defaultSize = 50000
    override def comparisons = List()
    def benchName = "map2";
    def apply(sz: Int, p: Int, w: String) = new Map2(sz, p, w)
  }
  
  class Map2(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    var result: Int = 0
    def comparisonMap = collection.Map()
    def runseq = {
      val r = this.seqcoll.asInstanceOf[collection.mutable.HashSet[T]].map(operators.mapper2)
      result = r.size
    }
    def runpar = {
      result = this.parcoll.map(operators.mapper2).size
    }
    def companion = Map2
    override def repetitionsPerRun = 50
    override def printResults {
      println("Size of last result: " + result)
    }
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
      val r = this.seqcoll.asInstanceOf[collection.mutable.HashSet[T]].map(operators.heavymapper)
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





object RefParHashTableSetBenches extends ParHashTableSetBenches[Dummy] {
  
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
    def runseq = for (x <- this.seqcoll) array(x.in) += 1
    def runpar = this.parcoll.pforeach { x => array(x.in) += 1 }
    def companion = ForeachSet
    
    override def onEnd {
      for (i <- 0 until array.length) {
        assert(array(i) == repetitionsPerRun * runs)
      }
    }
  }
  
  val operators = DummyOperators
  
  def createSequential(sz: Int, p: Int) = {
    val ht = new collection.mutable.HashSet[Dummy]
    for (i <- 0 until sz) ht += new Dummy(i)
    ht
  }
  
  def createParallel(sz: Int, p: Int) = {
    val phm = new ParHashSet[Dummy]
    for (i <- 0 until sz) phm += new Dummy(i)
    forkJoinPool.setParallelism(p)
    collection.parallel.tasksupport.environment = forkJoinPool
    phm
  }
  
}
