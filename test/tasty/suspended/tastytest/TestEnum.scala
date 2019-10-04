package tastytest

import java.lang.{Enum => _}
import Enums._

/** Suspended due to assertion error caused by having an arbitrary implicit parameter in a constructor with type params.
 */
object TestEnum {

  final class Colour(name: String, ordinal: Int) extends Enum[Colour](name, ordinal)

  object Colour extends EnumCompanion[Colour] {
    val Red   = new Colour("Red", 0)
    val Green = new Colour("Green", 1)
    val Blue  = new Colour("Blue", 2)
  }

  def test1 = assert(Colour.Red == Colour.Red)
  def test2 = assert(Colour.Green != Colour.Blue)
  def test3 = assert(Colour.Blue > Colour.Red)
  def test4 = assert(Colour.valueOf("Green") == Colour.Green)
  def test5 = assert(Colour.values `sameElements` Array(Colour.Red, Colour.Green, Colour.Blue))

  def main(args: Array[String]): Unit = {
    test1
    test2
    test3
    test4
    test5
    println("Suite passed!")
  }
}
