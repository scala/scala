

import scala.util.control.Breaks._



object Test {

  def main(args: Array[String]) {
    val x = tryBreakable {
      break
      2
    } catchBreak {
      3
    }
    assert(x == 3, x)

    val y = tryBreakable {
      2
    } catchBreak {
      3
    }
    assert(y == 2, y)

    val z = tryBreakable {
      break
      1.0
    } catchBreak {
      2
    }
    assert(z == 2.0, z)
  }

}
