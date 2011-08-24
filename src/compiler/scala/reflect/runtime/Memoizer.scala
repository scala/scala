package scala.reflect.runtime

import collection.mutable.ArrayBuffer
import Mirror.Type

/** Class that can be used for memoizing types in reified trees */
class Memoizer {
  private val mem = new ArrayBuffer[Mirror.Type]
  def get(n: Int): Type = mem(n)
  def add(n: Int, tpe: Type): Type = {
    while (mem.length <= n) mem += null
    mem(n) = tpe
    tpe
  }
}
