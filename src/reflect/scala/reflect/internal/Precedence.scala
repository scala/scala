/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.reflect.internal

import scala.annotation.switch
import Chars.{CodePoint, isOperatorPart, isScalaLetter}

final class Precedence private (val level: Int) extends AnyVal with Ordered[Precedence] {
  def compare(that: Precedence): Int = level.compare(that.level)
  override def toString = s"Precedence($level)"
}

object Precedence extends (Int => Precedence) {
  private[this] val ErrorName = "<error>"
  private def isAssignmentOp(name: String) = name match {
    case "!=" | "<=" | ">=" | "" => false
    case _                       => name.last == '=' && name.head != '=' && isOperatorPart(name.codePointAt(0))
  }
  private def firstChar(c: CodePoint): Precedence = apply((c: @switch) match {
    case '|'             => 2
    case '^'             => 3
    case '&'             => 4
    case '=' | '!'       => 5
    case '<' | '>'       => 6
    case ':'             => 7
    case '+' | '-'       => 8
    case '*' | '/' | '%' => 9
    case _               => if (isScalaLetter(c)) 1 else 10
  })

  def apply(level: Int): Precedence = new Precedence(level)
  def apply(name: String): Precedence = name match {
    case "" | ErrorName            => this(-1)
    case _ if isAssignmentOp(name) => this(0)
    case _                         => firstChar(name.codePointAt(0))
  }
}
