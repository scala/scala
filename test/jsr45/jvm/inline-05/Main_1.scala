// scalac: -opt:l:inline '-opt-inline-from:**'
// to enable the inliner log, add: -Yopt-log-inline _

// inline-05
object Main_1 {
  def main(args: Array[String]): Unit = {
    val result = Utils1_1.blackMagic(0)
    println(result)
  }
}
