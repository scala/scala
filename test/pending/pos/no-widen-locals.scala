// Worked from r23262 until that was reverted somewhere
// around r25016.
import annotation.switch

object Test {
  def f(x: Int) = {
    val X1 = 5
    val X2 = 10
    val X3 = 15
    val X4 = 20
    
    (x: @switch) match {
      case X1 => 1
      case X2 => 2
      case X3 => 3
      case X4 => 4
    }
  }
}
