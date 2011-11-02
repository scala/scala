package scala.collection.parallel.benchmarks


import scala.collection._
import scala.testing.Benchmark



trait BenchCompanion {
  def benchName: String
  def collectionName: String
  def fullname = collectionName + "." + benchName
  def defaultSize = 100000
  def comparisons = List[String]()
  def apply(sz: Int, parallelism: Int, what: String): Bench
}


/**
 * An interface for all benchmark classes.
 * A benchmark runs some functionality a prespecified number of times.
 */
trait Bench extends Benchmark {
  val size: Int
  
  val parallelism: Int
  
  val runWhat: String
  
  /**
   * Name of the benchmark. Convention is for it to start with the name of the collection being
   * tested, continuing '.' and ending with the name of the specific functionality being benchmarked. 
   * @return
   */
  def name: String = companion.fullname
  def collectionName: String = companion.collectionName
  def benchName: String = companion.benchName
  
  def companion: BenchCompanion
  
  def runseq: Unit
  
  def runpar: Unit
  
  /**
   * Describes the number of runs of the test.
   */
  val runs = 10
  
  /**
   * Returns the number of repetitions for this benchmark.
   */
  def repetitionsPerRun = 500
  
  /**
   * Resets the benchmark object. Typically, this means recreating
   * the collection being tested.
   */
  def reset: Unit
  
  /**
   * Returns a map of available comparison tests.
   */
  def comparisons: List[String] = companion.comparisons
  
  def comparison(name: String): Option[() => Unit] = comparisonMap.get(name)
  
  def comparisonMap: Map[String, () => Unit]
  
  def run = runWhat match {
    case "seq" => for (i <- 0 until repetitionsPerRun) runseq
    case "par" => for (i <- 0 until repetitionsPerRun) runpar
    case _ => comparison(runWhat) match {
      case Some(fun) => for (i <- 0 until repetitionsPerRun) fun()
      case None => throw new IllegalArgumentException("Unknown bench option: `" + runWhat + 
          "`, need `seq`, `par` or one of: " + comparisons.mkString("`", "`, `", "`"))
    }
  }
  
  /**
   * Prints results of the benchmark. May be overidden in benchmarks.
   */
  def printResults {}
  
  def onEnd {}
  
  def executeBenchmark = {
    println("-----------------------")
    print(name + ", " + runWhat + ", par.=" + parallelism + ", sz=" + niceSize + ": ")
    
    val times = runBenchmark(runs)
    
    onEnd
    
    for (t <- times) print(t + " ")
    println
    printResults
  }
  
  private def niceSize = if (size < 1000 || size % 1000 != 0) size.toString else size / 1000 + "k"
}


trait HavingResult[T] extends Bench {
  var runresult: T = null.asInstanceOf[T]
  
  abstract override def printResults {
    println("result: " + (if (runresult != null) runresult else "<not set>"))
    super.printResults
  }
}















