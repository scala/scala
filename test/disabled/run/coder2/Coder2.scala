

import collection.immutable._
import collection.parallel._//immutable._


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
  val wordsForNum: Map[String, Seq[String]] = 
    (words groupBy wordCode).map(t => (t._1, t._2.toSeq)) withDefaultValue Seq()
  
  val memo = collection.mutable.Map[String, Set[Seq[String]]]("" -> Set(Seq()))
  val wfnmemo = collection.mutable.Map[(String, String), Set[Seq[String]]]()
  val subsmemo = collection.mutable.Map[(String, String, String), Set[Seq[String]]]()
  
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
          val subsmapped = subs.map(rest => word +: rest)
          subsmemo += (number, number drop split, word) -> subsmapped
          subsmapped
        })
        wfnmemo += (number, number take split) -> wfn.toSet
        wfn
      })
      memo += number -> r
      r
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
  val wordsForNum: Map[String, ParSeq[String]] = 
    (words groupBy wordCode).map(t => (t._1, t._2.toSeq.par)) withDefaultValue ParSeq()
  
  val comparison = new SeqCoder(words)
  
  /** All ways to encode a number as a list of words */
  def encode(number: String): ParSet[ParSeq[String]] = 
    if (number.isEmpty) ParSet(ParSeq())
    else {
      val splits = (1 to number.length).toSet.par
      // for {
      //   split <- splits
      //   word <- wordsForNum(number take split)
      //   rest <- encode(number drop split)
      // } yield word :: rest
      val r = splits.flatMap(split => {
        val wfn = wordsForNum(number take split).flatMap(word => {
          val subs = encode(number drop split)
          assertNumber(number drop split, subs)
          val subsmapped = subs.map(rest => word +: rest)
          assertSubs(number, number drop split, word, subsmapped)
          subsmapped
        })
        assertWfn(number, number take split, number drop split, wfn)
        wfn
      })
      assertNumber(number, r)
      r
    }
  
  def assertSubs(num: String, subsfrom: String, word: String, r: ParSet[ParSeq[String]]) {
    val m = comparison.subsmemo((num, subsfrom, word))
    if (r != m) {
      println("map for number from subs and word: " + num + ", " + subsfrom + ", " + word)
      println("parset: " + r.size)
      println("memoed: " + m.size)
      error("r != m")
    }
  }
  
  def assertWfn(num: String, split: String, dropped: String, r: ParSeq[ParSeq[String]]) {
    val m = comparison.wfnmemo((num, split))
    val rs = r.toSet.par
    val words: ParSeq[String] = wordsForNum(split)
    if (rs != m) {
      println("flatmap for number with split: " + num + ", " + split)
      println("words for: " + words)
      println("parset: " + rs.size)
      println("memoed: " + m.size)
      println("retrying...")
      for (i <- 0 until 30) {
        val r2: ParSeq[ParSeq[String]] = words.flatMap(word => {
          val subs: ParSet[ParSeq[String]] = encode(dropped)
          println("subs size for '" + dropped + "': " + subs.size)
          val subsmapped: ParSet[ParSeq[String]] = subs.map(rest => word +: rest)
          println("map size: " + subsmapped.size)
          subsmapped.toList
        })
        println(i + ") retry size: " + r2.size)
      }
      error("rs != m")
    }
  }
  
  def assertNumber(num: String, r: ParSet[ParSeq[String]]) {
    val m = comparison.memo(num)
    if (r != m) {
      println("for number: " + num)
      println("parset: " + r.size)
      println("memoed: " + m.size)
      error("r != m")
    }
  }
    
  /** Maps a number to a list of all word phrases that can 
   *  represent it */
  def translate(number: String): ParSet[String] = {
    comparison.translate(number)
    encode(number) map (_.seq mkString " ")
  }
  
  def ??? : Nothing = throw new UnsupportedOperationException
}


/** Test code */
object Test {
  val code = "2328437472947"//36262633"//837976"//"6477323986225453446"
  //val code = "747294736262633"
  
  /* */
  def main(args : Array[String]) {
    for (i <- 0 until 10) {
      val seqcoder = new SeqCoder(Dictionary.wordlist)
      val sts = seqcoder.translate(code)
      //println("Translation check: " + st.size)
      
      val parcoder = new ParCoder(Dictionary.wordlist)
      val pts = parcoder.translate(code)
      //println("Translation check: " + pt.size)
      
      val st = sts.toList.sorted
      val pt = pts.toList.sorted
      if (st.size != pt.size) {
        val zipped = st.zip(pt)
        val ind = zipped.indexWhere { case (a, b) => a != b }
        val sliced = zipped.slice(ind - 10, ind + 10)
        //println(sliced.map(t => t._1 + "\n" + t._2 + "\n--------").mkString("\n"))
        //println(i + ") seq vs par: " + st.size + " vs " + pt.size)
      }
      if (st != pt) {
        val zipped = (st.toList.sorted zip pt.toList.sorted);
        val diffp = zipped indexWhere { case (x, y) => x != y }
        //println(zipped/*.slice(diffp - 10, diffp + 10)*/ mkString ("\n"))
        //println((st.toList.sorted zip pt.toList.sorted) map { case (x, y) => (x == y) } reduceLeft(_ && _))
      }
      assert(st == pt)
    }
  }
}














