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

class B extends Dynamic {
  def applyDynamic[T1](name: String)(x: T1): Int = 1
  def applyDynamic[T1, T2](name: String)(x: T1, y: T2): Int = 2
  def applyDynamic[T1, T2](name: String)(x: String, y: T1, z: T2): Int = 3
}
