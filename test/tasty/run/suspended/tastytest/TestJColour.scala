package tastytest

/** Suspended as looking for a non-existing ctor with sig (1)java.lang.Enum causes a compiler crash
 */
object TestJColour {

  def test1 = assert(JColour.Red == JColour.Red)
  def test2 = assert(JColour.Green != JColour.Blue)
  def test3 = assert(JColour.Blue.compareTo(JColour.Red) > 0)

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    println("Suite passed!")
  }
}
