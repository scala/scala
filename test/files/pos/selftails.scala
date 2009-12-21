package net.liftweb.util

/**
* This trait adds functionality to Scala standard types
*/
trait BasicTypesHelpers { self: StringHelpers with ControlHelpers =>

  /**
   * Compare two arrays of Byte for byte equality.
   * @return true if two Byte arrays contain the same bytes
   */
  def isEq(a: Array[Byte], b: Array[Byte]) = {
    def eq(a: Array[Byte], b: Array[Byte], pos: Int, len: Int): Boolean = {
      if (pos == len) true
      else if (a(pos) != b(pos)) false
      else eq(a , b, pos + 1, len)
    }
    a.length == b.length && eq(a, b, 0, a.length)
  }
}

trait StringHelpers
trait ControlHelpers
