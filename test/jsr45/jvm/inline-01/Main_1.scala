// scalac: -opt:l:inline '-opt-inline-from:**'
// to enable the inliner log, add: -Yopt-log-inline _
import Utils_1._

class Main_1 {
  def foo(): Unit = {
    val myList = List(1, 2, 3, 4, 50, 60, 70, 80)
    val bmList = myList.map(blackMagic)
  }
}
