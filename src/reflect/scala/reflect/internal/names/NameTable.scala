package scala.reflect.internal.names

import java.nio.charset.Charset

abstract class NameTable[T <: AnyRef] {
  def size: Int
  def find(source: String): T

  //there is some room to optimise here
  def find(chars: Array[Char], start: Int, count: Int): T =
    find(new String(chars, start, count))
}

object NameTable {
  val charSet = Charset.forName("UTF-8")
}