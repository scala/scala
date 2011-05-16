package scala.reflect
package std

trait Positions {

  class Position {
    def focus: Position = this
    def isRange: Boolean = false
    def show: String = toString
  }

  val NoPosition: Position = new Position
}
