import scala.tools.partest.IcodeTest

class Switches {
  private[this] final val ONE = 1

  def switchBad(i: Byte): Int = i match {
    case ONE => 1
    case 2 => 2
    case 3 => 3
    case _ => 0
  }

  def switchOkay(i: Byte): Int = i match {
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case _ => 0
  }
}

object Test extends IcodeTest {
  // ensure we get two switches out of this -- ignore the rest of the output for robustness
  // exclude the constant we emit for the "SWITCH ..." string below (we get the icode for all the code you see in this file)
  override def show() = println(collectIcode("").filter(x => x.indexOf("SWITCH ...") >= 0 && x.indexOf("CONSTANT(") == -1).size)
}

