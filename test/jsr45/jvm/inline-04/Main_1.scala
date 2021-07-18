// scalac: -opt:l:inline '-opt-inline-from:**'
// to enable the inliner log, add: -Yopt-log-inline _
import Utils1._
import Utils2._

// inline-04
object Main_1 {
  def main(args: Array[String]): Unit = {
    val result1 = applyToInt(1, blackMagic1(_, 5))
    val result2 = applyToInt(1, blackMagic2(_, 10))
    println(result1 + result2)
  }
}