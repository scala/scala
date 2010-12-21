package scala.collection.parallel.benchmarks
package misc


import collection.immutable._
import collection.parallel.immutable._


class SeqCoder(words: List[String]) {

  private val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /** Invert the mnemnonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] =
    for ((digit, letters) <- m; letter <- letters) yield letter -> digit

  /** Maps a word to the digit string it represents,
   * e.g. `Java` -> `5282`  */
  private def wordCode(word: String): String = word.toUpperCase map charCode

  /** A map from digit strings to the words that represent
   *  them e.g. `5282` -> List(`Java`, `Kata`, `Lava`, ...)
   */
  val wordsForNum: Map[String, List[String]] =
    words groupBy wordCode withDefaultValue List()

  /** All ways to encode a number as a list of words */
  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      val splits = (1 to number.length).toSet
      for {
	split <- splits
	word <- wordsForNum(number take split)
	rest <- encode(number drop split)
      } yield word :: rest
    }

  /** Maps a number to a list of all word phrases that can
   *  represent it */
  def translate(number: String): Set[String] = encode(number) map (_ mkString " ")

  def ??? : Nothing = throw new UnsupportedOperationException
}

class ParCoder(words: List[String]) {

  private val m = Map(
      '2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
      '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /** Invert the mnemnonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] =
    for ((digit, letters) <- m; letter <- letters) yield letter -> digit

  /** Maps a word to the digit string it represents,
   * e.g. `Java` -> `5282`  */
  private def wordCode(word: String): String = word.toUpperCase map charCode

  /** A map from digit strings to the words that represent
   *  them e.g. `5282` -> List(`Java`, `Kata`, `Lava`, ...)
   */
  val wordsForNum: Map[String, List[String]] =
    words groupBy wordCode withDefaultValue List()

  /** All ways to encode a number as a list of words */
  def encode(number: String): ParSet[List[String]] =
    if (number.isEmpty) ParSet(List())
    else {
      val splits = (1 to number.length).toParSet
      for {
	split <- splits
	word <- wordsForNum(number take split)
	rest <- encode(number drop split)
      } yield word :: rest
    }

  /** Maps a number to a list of all word phrases that can
   *  represent it */
  def translate(number: String): ParSet[String] = encode(number) map (_ mkString " ")

  def ??? : Nothing = throw new UnsupportedOperationException
}


/** Test code */
object Main {
  def main(args : Array[String]) : Unit = {
    val coder = new SeqCoder(List("Scala", "Python", "Ruby", "Java", "Kata", "Lava", "a", "rocks", "pack", "rack", "sucks", "works"))
    println(coder.wordsForNum)
    println(coder.translate("7225276257"))
  }
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

  val code = "2328437472947362626"//33"//837976"//"6477323986225453446"

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
      parcoder = new ParCoder(Dictionary.wordlist)
      val t = parcoder.translate(code)
      println("Translation check: " + t.size)
      //println(t)
  }

  def comparisonMap = Map()

}

