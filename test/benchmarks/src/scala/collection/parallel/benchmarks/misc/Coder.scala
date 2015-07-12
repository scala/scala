package scala.collection.parallel.benchmarks
package misc






import collection._ //immutable._
import collection.parallel._//immutable._


class SeqCoder(words: List[String]) {
  
  private val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", 
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
      
  /** Invert the mnemonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] = 
    for ((digit, letters) <- m; letter <- letters) yield letter -> digit
    
  /** Maps a word to the digit string it represents, 
   * e.g. `Java` -> `5282`  */
  private def wordCode(word: String): String = word.toUpperCase map charCode
    
  /** A map from digit strings to the words that represent 
   *  them e.g. `5282` -> List(`Java`, `Kata`, `Lava`, ...)
   */
  val wordsForNum: Map[String, Seq[String]] = 
    (words groupBy wordCode).map(t => (t._1, t._2.toSeq)) withDefaultValue Seq()
  
  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[Seq[String]] = 
    if (number.isEmpty) Set(Seq())
    else {
      val splits = (1 to number.length).toSet
      // for {
      //   split <- splits
      //   word <- wordsForNum(number take split)
      //   rest <- encode(number drop split)
      // } yield word :: rest
      val r = splits.flatMap(split => {
        val wfn = wordsForNum(number take split).flatMap(word => {
          val subs = encode(number drop split)
          subs.map(rest => word +: rest)
        })
        wfn
      })
      r
    }
    
  /** Maps a number to a list of all word phrases that can 
   *  represent it */
  def translate(number: String) = encode(number)// map (_ mkString " ")
  
  def ??? : Nothing = throw new UnsupportedOperationException
}

class ParCoder(words: List[String]) {
  
  private val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL", 
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")
      
  /** Invert the mnemonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] = 
    for ((digit, letters) <- m; letter <- letters) yield letter -> digit
    
  /** Maps a word to the digit string it represents, 
   * e.g. `Java` -> `5282`  */
  private def wordCode(word: String): String = word.toUpperCase map charCode
    
  /** A map from digit strings to the words that represent 
   *  them e.g. `5282` -> List(`Java`, `Kata`, `Lava`, ...)
   */
  val wordsForNum: Map[String, Seq[String]] = 
    (words groupBy wordCode).map(t => (t._1, t._2)) withDefaultValue Seq()
  
  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[Seq[String]] = if (number.length > 12) {
    if (number.isEmpty) ParSet(ParSeq())
    else {
      val splits = (1 to number.length).toParSet
      for {
        split <- splits
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word +: rest
    }
  } else {
    if (number.isEmpty) Set(Seq())
    else {
      val splits = (1 to number.length).toSet
      for {
        split <- splits
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word +: rest
    }
  }
  
  /** Maps a number to a list of all word phrases that can 
   *  represent it */
  def translate(number: String) = {
    encode(number)// map (_ mkString " ")
  }
  
  def ??? : Nothing = throw new UnsupportedOperationException
}





object Coder extends BenchCompanion {
  def benchName = "Coder"
  def collectionName = "General"
  def apply(sz: Int, p: Int, what: String) = new Coder(sz, p, what)
  override def defaultSize = 100
}

class Coder(val size: Int, val parallelism: Int, val runWhat: String) extends Bench {
  def companion = Coder
  
  var seqcoder: SeqCoder = null
  var parcoder: ParCoder = null
  
  override def repetitionsPerRun = 1
  
  val code = "23284374729473626268379762538"
  
  reset
  
  def runseq {
    val translations = seqcoder.translate(code)
    //println(translations)
  }
  
  def runpar {
    val translations = parcoder.translate(code)
    //println(translations)
  }
  
  def reset = runWhat match {
    case "seq" =>
      seqcoder = new SeqCoder(Dictionary.wordlist)
      val t = seqcoder.translate(code)
      println("Translation check: " + t.size)
      //println(t)
    case "par" =>
      collection.parallel.tasksupport.environment.asInstanceOf[concurrent.forkjoin.ForkJoinPool].setParallelism(parallelism)
      parcoder = new ParCoder(Dictionary.wordlist)
      val t = parcoder.translate(code)
      println("Translation check: " + t.size)
      //println(t)
  }
  
  def comparisonMap = Map()
  
}

