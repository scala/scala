// scalac: -opt:l:inline '-opt-inline-from:**'
// to enable the inliner log, add: -Yopt-log-inline _
import Utils_1._

// inline-03
object Main_1 {
  def main(args: Array[String]): Unit = {
    val result = applyToInt(1, blackMagic(_, 5))
    println(result)
  }
}