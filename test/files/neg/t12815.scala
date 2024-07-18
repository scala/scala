//> using options -Werror

import scala.beans.BeanProperty

trait T {
  def getAa: String
  def getBb(): String

  def c: Int
  def d(): Int

  def e: Int
  def f(): Int
}
class C extends T {
  @BeanProperty val aa: String = "" // ok
  @BeanProperty val bb: String = ""

  val c: Int = 1
  val d: Int = 1 // ok

  def e(): Int = 1 // warn
  def f: Int = 1   // warn
}
