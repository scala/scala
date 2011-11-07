package scala.collection.parallel.benchmarks.parallel_range





import scala.collection.parallel.benchmarks.generic._
import scala.collection.parallel.immutable.ParRange
import scala.collection.parallel.benchmarks.generic.StandardParIterableBenches





object RangeBenches extends StandardParIterableBenches[Int, ParRange] {
  
  def nameOfCollection = "ParRange"
  def operators = new IntOperators {}
  def comparisonMap = collection.Map()
  val forkJoinPool = new scala.concurrent.forkjoin.ForkJoinPool
  def createSequential(sz: Int, p: Int) = new collection.immutable.Range(0, sz, 1)
  def createParallel(sz: Int, p: Int) = {
    val pr = collection.parallel.immutable.ParRange(0, sz, 1, false)
    forkJoinPool.setParallelism(p)
    collection.parallel.tasksupport.environment = forkJoinPool
    pr
  }
  
  object MapLight extends IterableBenchCompanion {
    override def defaultSize = 20000
    def benchName = "map-light";
    def apply(sz: Int, p: Int, w: String) = new MapLight(sz, p, w)
  }
  
  class MapLight(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def calc(n: Int) = n % 2 + 1
    
    def comparisonMap = collection.Map()
    def runseq = for (n <- this.seqcoll) yield calc(n)
    def runpar = for (n <- this.parcoll) yield calc(n)
    def companion = MapLight
  }
  
  object MapMedium extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "map-medium";
    def apply(sz: Int, p: Int, w: String) = new MapMedium(sz, p, w)
  }
  
  class MapMedium(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def calc(n: Int) = {
      var i = 0
      var sum = n
      while (i < 40) {
        i += 1
        sum += n % i
      }
      sum
    }
    
    def comparisonMap = collection.Map()
    def runseq = for (n <- this.seqcoll) yield calc(n)
    def runpar = for (n <- this.parcoll) yield calc(n)
    def companion = MapMedium
  }
  
  object ForeachModify extends IterableBenchCompanion {
    override def defaultSize = 150000
    def benchName = "foreach-modify";
    def apply(sz: Int, p: Int, w: String) = new ForeachModify(sz, p, w)
  }
  
  class ForeachModify(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val array = new Array[Int](size)
    def modify(n: Int) = array(n) += 1
    
    def comparisonMap = collection.Map()
    def runseq = for (n <- this.seqcoll) modify(n)
    def runpar = for (n <- this.parcoll.asInstanceOf[ParRange]) {
      modify(n)
      ()
    }
    def companion = ForeachModify
  }
  
  object ForeachModifyMedium extends IterableBenchCompanion {
    override def defaultSize = 20000
    def benchName = "foreach-modify-medium";
    def apply(sz: Int, p: Int, w: String) = new ForeachModifyMedium(sz, p, w)
  }
  
  class ForeachModifyMedium(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val array = new Array[Int](size)
    def modify(n: Int) = array(n) = {
      var i = 0
      var sum = 0
      while (i < 15) {
        sum += i % 3
        i += i + 1
      }
      sum
    }
    
    def comparisonMap = collection.Map()
    def runseq = for (n <- this.seqcoll) modify(n)
    def runpar = for (n <- this.parcoll) modify(n)
    def companion = ForeachModifyMedium
  }
  
  object ForeachModifyHeavy extends IterableBenchCompanion {
    override def defaultSize = 1000
    def benchName = "foreach-modify-heavy";
    def apply(sz: Int, p: Int, w: String) = new ForeachModifyHeavy(sz, p, w)
  }
  
  class ForeachModifyHeavy(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val array = new Array[Int](size)
    def modify(n: Int) = array(n) = collatz(10000 + array(n))
    
    def comparisonMap = collection.Map()
    def runseq = for (n <- this.seqcoll) modify(n)
    def runpar = for (n <- this.parcoll) modify(n)
    def companion = ForeachModifyHeavy
  }
  
  object ForeachAdd extends IterableBenchCompanion {
    override def defaultSize = 10000
    def benchName = "foreach-add";
    def apply(sz: Int, p: Int, w: String) = new ForeachAdd(sz, p, w)
    override def comparisons = List("seq-hashmap")
  }
  
  class ForeachAdd(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val cmap = new java.util.concurrent.ConcurrentHashMap[Int, Int]
    val hmap = new java.util.HashMap[Int, Int]
    
    override def reset = runWhat match {
      case "seq-hashmap" => seqcoll = createSequential(size, parallelism)
      case _ => super.reset
    }
    
    def comparisonMap = collection.Map("seq-hashmap" -> runseqhashmap _)
    def runseqhashmap = for (i <- seqcoll) hmap put (i, onesum(i))
    def runseq = for (i <- seqcoll) cmap put (i, onesum(i))
    def runpar = for (i <- parcoll) cmap put (i, onesum(i))
    def companion = ForeachAdd
  }
  
  object ForeachAddCollatz extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "foreach-add-collatz";
    def apply(sz: Int, p: Int, w: String) = new ForeachAddCollatz(sz, p, w)
    override def comparisons = List("seq-hashmap")
  }
  
  class ForeachAddCollatz(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    val cmap = new java.util.concurrent.ConcurrentHashMap[Int, Int]
    val hmap = new java.util.HashMap[Int, Int]
    
    override def reset = runWhat match {
      case "seq-hashmap" => seqcoll = createSequential(size, parallelism)
      case _ => super.reset
    }
    
    def comparisonMap = collection.Map("seq-hashmap" -> runseqhashmap _)
    def runseqhashmap = for (i <- seqcoll) hmap put (i, collatz(i))
    def runseq = for (i <- seqcoll) cmap put (i, collatz(i))
    def runpar = for (i <- parcoll) cmap put (i, collatz(i))
    def companion = ForeachAddCollatz
  }
  
  def collatz(n: Int) = {
    var curr = n
    var sum = 0
    while (curr > 1) {
      sum += curr
      if (curr % 2 == 0) curr = curr / 2
      else curr = curr * 3 + 1
    }
    sum
  }
  
  def onesum(n: Int) = {
    var left = n
    var sum = 0
    while (left > 0) {
      sum += left % 2
      left /= 2
    }
    sum
  }
  
}











