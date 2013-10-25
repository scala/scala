package scala
package reflect
package internal

import scala.annotation.switch
import Chars._

final class Precedence private (val level: Int) extends AnyVal with Ordered[Precedence] {
  def compare(that: Precedence): Int = level compare that.level
  override def toString = s"Precedence($level)"
}


object Precedence extends (Int => Precedence) {
  private val ErrorName = "<error>"
  private def isAssignmentOp(name: String) = name match {
    case "!=" | "<=" | ">=" | "" => false
    case _                       => name.last == '=' && name.head != '=' && isOperatorPart(name.head)
  }
  private def firstChar(ch: Char): Precedence = apply((ch: @switch) match {
    case '|'             => 2
    case '^'             => 3
    case '&'             => 4
    case '=' | '!'       => 5
    case '<' | '>'       => 6
    case ':'             => 7
    case '+' | '-'       => 8
    case '*' | '/' | '%' => 9
    case _               => if (isScalaLetter(ch)) 1 else 10
  })

  def apply(level: Int): Precedence = new Precedence(level)
  def apply(name: String): Precedence = name match {
    case "" | ErrorName            => this(-1)
    case _ if isAssignmentOp(name) => this(0)
    case _                         => firstChar(name charAt 0)
  }
}
