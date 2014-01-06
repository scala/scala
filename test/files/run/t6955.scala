import scala.tools.partest.IcodeComparison

// this class should compile to code that uses switches (twice)
class Switches {
  type Tag = Byte

  def switchBad(i: Tag): Int = i match { // notice type of i is Tag = Byte
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case _ => 0
  }

  // this worked before, should keep working
  def switchOkay(i: Byte): Int = i match {
    case 1 => 1
    case 2 => 2
    case 3 => 3
    case _ => 0
  }
}

object Test extends IcodeComparison {
  // ensure we get two switches out of this -- ignore the rest of the output for robustness
  // exclude the constant we emit for the "SWITCH ..." string below (we get the icode for all the code you see in this file)
  override def show() = {
    val expected = 2
    val actual = (collectIcode() filter {
      x => x.indexOf("SWITCH ...") >= 0 && x.indexOf("CONSTANT(") == -1
    }).size
    assert(actual == expected)
  }
}

