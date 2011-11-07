package scala.collection.parallel.benchmarks
package misc






import collection._ //immutable._
import collection.parallel._//immutable._







object Loader extends BenchCompanion {
  def benchName = "Loader"
  def collectionName = "General"
  def apply(sz: Int, p: Int, what: String) = new Loader(sz, p, what)
  override def defaultSize = 100
}


class Loader(val size: Int, val parallelism: Int, val runWhat: String) extends Bench {
  def companion = Loader
  
  override def repetitionsPerRun = 1
  
  reset
  
  val wa = Dictionary.wordarray ++ Dictionary.wordarray ++ Dictionary.wordarray
  
  def runseq {
    val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", 
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )
    val charCode: Map[Char, Char] = for ((digit, letters) <- m; letter <- letters) yield letter -> digit
    def wordCode(word: String): String = (word.toUpperCase.toList map charCode).toString
    
    wa groupBy wordCode
  }
  
  def runpar {
    val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", 
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ"
    )
    val charCode: Map[Char, Char] = for ((digit, letters) <- m; letter <- letters) yield letter -> digit
    def wordCode(word: String): String = (word.toUpperCase.toList map charCode).toString
    
    wa.par groupBy wordCode
  }
  
  def reset = runWhat match {
    case "seq" =>
    case "par" =>
      collection.parallel.tasksupport.environment.asInstanceOf[concurrent.forkjoin.ForkJoinPool].setParallelism(parallelism)
  }
  
  def comparisonMap = Map()
  
}

