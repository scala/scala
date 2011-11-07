package scala.collection.parallel
package benchmarks
package generic



import scala.collection.SeqView



trait ParIterableBenches[T, Coll <: ParIterable[T]] {
self =>
  
  def createSequential(sz: Int, p: Int): Iterable[T]
  def createParallel(sz: Int, p: Int): Coll
  def nameOfCollection: String
  def operators: Operators[T]
    
  trait IterableBenchCompanion extends BenchCompanion {
    def collectionName = self.nameOfCollection
  }
  
  trait IterableBench extends collection.parallel.benchmarks.Bench {
    protected var seqcoll: Iterable[T] = null
    protected var parcoll: Coll = null.asInstanceOf[Coll]
    
    reset
    
    def reset = runWhat match {
      case "seq" => this.seqcoll = createSequential(size, parallelism)
      case "par" => this.parcoll = createParallel(size, parallelism)
      case _ =>
    }
    
    def nameOfCollection = self.nameOfCollection
    def operators = self.operators
    def createSequential(sz: Int, p: Int) = self.createSequential(size, parallelism)
    def createParallel(sz: Int, p: Int) = self.createParallel(size, parallelism)
    def forkJoinPool: scala.concurrent.forkjoin.ForkJoinPool = self.forkJoinPool
    
    override def printResults {
      println(" --- Fork join pool state --- ")
      println("Parallelism: " + forkJoinPool.getParallelism)
      println("Active threads: " + forkJoinPool.getActiveThreadCount)
      println("Work stealings: "  + forkJoinPool.getStealCount)
    }
    
  }
  
  def forkJoinPool: scala.concurrent.forkjoin.ForkJoinPool
    
}


trait ParSeqBenches[T, Coll <: ParSeq[T]] extends ParIterableBenches[T, Coll] {
self =>
  
  def createSequential(sz: Int, p: Int): Seq[T]
  
  trait SeqBenchCompanion extends BenchCompanion {
    def collectionName = self.nameOfCollection
  }
  
  trait SeqBench extends IterableBench {
    def seqcollAsSeq = seqcoll.asInstanceOf[Seq[T]]
    override def createSequential(sz: Int, p: Int) = self.createSequential(sz, p)
  }
  
}




/** Standard benchmarks for collections.
 */ 
trait StandardParIterableBenches[T, Coll <: ParIterable[T]] extends ParIterableBenches[T, Coll] {
  
  object Reduce extends IterableBenchCompanion {
    override def defaultSize = 50000
    def benchName = "reduce";
    def apply(sz: Int, p: Int, w: String) = new Reduce(sz, p, w)
  }
  
  class Reduce(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.reducer)
    def runpar = this.parcoll.reduce(operators.reducer)
    def companion = Reduce
  }
  
  object ReduceMedium extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "reduce-medium";
    def apply(sz: Int, p: Int, w: String) = new ReduceMedium(sz, p, w)
  }
  
  class ReduceMedium(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.mediumreducer)
    def runpar = this.parcoll.reduce(operators.mediumreducer)
    def companion = ReduceMedium
  }
  
  object Map extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "map";
    def apply(sz: Int, p: Int, w: String) = new Map(sz, p, w)
  }
  
  class Map(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.map(operators.mapper)
    def runpar = this.parcoll.map(operators.mapper)
    def companion = Map
  }
  
  object Filter extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "filter";
    def apply(sz: Int, p: Int, w: String) = new Filter(sz, p, w)
  }
  
  class Filter(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.filter(operators.filterer)
    def runpar = this.parcoll.filter(operators.filterer)
    def companion = Filter
  }
  
  object FlatMap extends IterableBenchCompanion {
    override def defaultSize = 5000
    def benchName = "flatmap";
    def apply(sz: Int, p: Int, w: String) = new FlatMap(sz, p, w)
  }
  
  class FlatMap(val size: Int, val parallelism: Int, val runWhat: String)
  extends IterableBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.flatMap(operators.flatmapper)
    def runpar = this.parcoll.flatMap(operators.flatmapper)
    def companion = FlatMap
  }  
  
}



/** Benchmarks for sequence views.
 */
trait ParSeqViewBenches[T, Coll <: ParSeqView[T, ParSeq[T], CollSeq], CollSeq] extends ParSeqBenches[T, Coll] {
self =>
  
  trait SeqViewBench extends SeqBench {
    lazy val seqview: SeqView[T, Seq[T]] = createSeqView(size, parallelism)
    
    def createSeqView(sz: Int, p: Int) = self.createSeqView(sz, p)
  }
  
  def createSeqView(sz: Int, p: Int): SeqView[T, Seq[T]]
    
  object Iteration extends SeqBenchCompanion {
    override def defaultSize = 250000
    def benchName = "iter"
    def apply(sz: Int, p: Int, w: String) = new Iteration(sz, p, w)
  }
  
  class Iteration(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = this.seqcoll.foreach(operators.eachFun)
    def runpar = this.parcoll.pforeach(operators.eachFun)
    def runseqview = {
      this.seqview.foreach(operators.eachFun)
    }
    def companion = Iteration
  }
  
  object IterationS extends SeqBenchCompanion {
    override def defaultSize = 250000
    def benchName = "iter-s"
    def apply(sz: Int, p: Int, w: String) = new IterationS(sz, p, w)
  }
  
  class IterationS(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = this.seqcoll.slice(0, size / 2).foreach(operators.eachFun)
    def runpar = this.parcoll.slice(0, size / 2).pforeach(operators.eachFun)
    def runseqview = this.seqview.slice(0, size / 2).foreach(operators.eachFun)
    def companion = IterationS
  }

  object IterationM extends SeqBenchCompanion {
    override def defaultSize = 100000
    def benchName = "iter-m"
    def apply(sz: Int, p: Int, w: String) = new IterationM(sz, p, w)
  }
  
  class IterationM(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = this.seqcoll.map(operators.mapper).foreach(operators.eachFun)
    def runpar = this.parcoll.map(operators.mapper).pforeach(operators.eachFun)
    def runseqview = this.seqview.map(operators.mapper).foreach(operators.eachFun)
    def companion = IterationM
  }
  
  object IterationA extends SeqBenchCompanion {
    override def defaultSize = 50000
    def benchName = "iter-a"
    def apply(sz: Int, p: Int, w: String) = new IterationA(sz, p, w)
  }
  
  class IterationA(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    val appended = operators.sequence(size)
    val sqappended = appended.toSeq
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = {
      val withapp = this.seqcoll.++(sqappended)
      withapp.foreach(operators.eachFun)
    }
    def runpar = this.parcoll.++(appended).pforeach(operators.eachFun)
    def runseqview = this.seqview.++(appended).foreach(operators.eachFun)
    def companion = IterationA
  }
  
  object IterationZ extends SeqBenchCompanion {
    override def defaultSize = 50000
    def benchName = "iter-z"
    def apply(sz: Int, p: Int, w: String) = new IterationZ(sz, p, w)
  }
  
  class IterationZ(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    val zipped = operators.sequence(size)
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = {
      val withzip = this.seqcoll.zip(zipped)
      withzip.foreach(operators.eachPairFun)
    }
    def runpar = this.parcoll.zip(zipped).pforeach(operators.eachPairFun)
    def runseqview = this.seqview.zip(zipped).foreach(operators.eachPairFun)
    def companion = IterationZ
  }
  
  object IterationP extends SeqBenchCompanion {
    override def defaultSize = 50000
    def benchName = "iter-p"
    def apply(sz: Int, p: Int, w: String) = new IterationP(sz, p, w)
  }
  
  class IterationP(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    val patch = operators.sequence(size / 4)
    val sqpatch = patch.toSeq
    def comparisonMap = collection.Map("seqview" -> runseqview _)
    def runseq = {
      val withpatch = this.seqcollAsSeq.patch(size / 4, sqpatch, size / 2)
      withpatch.foreach(operators.eachFun)
    }
    def runpar = this.parcoll.patch(size / 4, patch, size / 2).pforeach(operators.eachFun)
    def runseqview = this.seqview.patch(size / 4, patch, size / 2).foreach(operators.eachFun)
    def companion = IterationP
  }
    
  object Reduce extends SeqBenchCompanion {
    override def defaultSize = 50000
    def benchName = "reduce";
    def apply(sz: Int, p: Int, w: String) = new Reduce(sz, p, w)
  }
  
  class Reduce(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.reducer)
    def runpar = this.parcoll.reduce(operators.reducer)
    def companion = Reduce
  }
  
  object MediumReduce extends SeqBenchCompanion {
    override def defaultSize = 50000
    def benchName = "reduce-medium";
    def apply(sz: Int, p: Int, w: String) = new MediumReduce(sz, p, w)
  }
  
  class MediumReduce(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    def comparisonMap = collection.Map()
    def runseq = this.seqcoll.reduceLeft(operators.mediumreducer)
    def runpar = this.parcoll.reduce(operators.mediumreducer)
    def companion = Reduce
  }
  
  object ModifyThenReduce extends SeqBenchCompanion {
    override def defaultSize = 20000
    def benchName = "modify-then-reduce";
    def apply(sz: Int, p: Int, w: String) = new ModifyThenReduce(sz, p, w)
  }
  
  class ModifyThenReduce(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    val toadd = createSequential(size, parallelism)
    def comparisonMap = collection.Map()
    def runseq = {
      val modified = (seqcoll ++ toadd).drop(size).map(operators.mapper).++(toadd).take(size)
      modified.reduceLeft(operators.reducer)
    }
    def runpar = {
      val modified = (parcoll ++ toadd).drop(size).map(operators.mapper).++(toadd).take(size)
      modified.reduce(operators.reducer)
    }
    def companion = ModifyThenReduce
  }
  
  object ModifyThenForce extends SeqBenchCompanion {
    override def defaultSize = 20000
    def benchName = "modify-then-force";
    def apply(sz: Int, p: Int, w: String) = new ModifyThenForce(sz, p, w)
  }
  
  class ModifyThenForce(val size: Int, val parallelism: Int, val runWhat: String)
  extends SeqBench with SeqViewBench {
    val toadd = createSequential(size, parallelism)
    def comparisonMap = collection.Map()
    def runseq = (seqcoll ++ toadd).drop(size).map(operators.mapper).++(toadd).take(size)
    def runpar = {
      val r: ParSeqView[T, ParSeq[T], Seq[T]] = (parcoll ++ toadd).drop(size).map(operators.mapper).++(toadd).take(size)
      r.force
    }
    def companion = ModifyThenForce
  }
  
}
























