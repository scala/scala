package foo

import scala.language.dynamics

class DoesntExtendDynamic {
  def applyDynamic(name: String)(s: String): Int = 1
  def applyDynamic(name: String)(x: Int): Int = 2
}

class A extends Dynamic {
  def applyDynamic(name: String)(s: String): Int = 1
  def applyDynamic(name: String)(x: Int): Int = 2
}
