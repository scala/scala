// scalac: -opt:l:inline '-opt-inline-from:**'
// to enable the inliner log, add: -Yopt-log-inline _
import Utils_1._
import Tools_1._

// inline-02
class Main_1 {
  def foo(): Unit = {
    val myList = List(1, 2, 3, 4, 50, 60, 70, 80)
    val bmList = blackMagic(myList)

    // dummy lines
    // dummy lines
    // dummy lines

    val myList2 = List(1, 2, 3, 4, 50, 60, 70, 80)
    val bmList2 = blackMagic(myList)

    // dummy lines
    // dummy lines
    // dummy lines

    val x = 42
    val xx = aura(x)
  }
}
