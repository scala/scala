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

package scala.annotation

/** An annotation to be applied to a match expression.  If present,
 *  the compiler will verify that the match has been compiled to a
 *  [[https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-3.html#jvms-3.10 tableswitch or lookupswitch]]
 *  and issue a warning if it instead compiles into a series of conditional expressions.
 *  Example usage:
{{{
  val Constant = 'Q'
  def tokenMe(ch: Char) = (ch: @switch) match {
    case ' ' | '\t' | '\n'  => 1
    case 'A' | 'Z' | '$'    => 2
    case '5' | Constant     => 3  // a non-literal may prevent switch generation: this would not compile
    case _                  => 4
  }
}}}
 *
 *  Note: for pattern matches with one or two cases, the compiler generates jump instructions.
 *  Annotating such a match with `@switch` does not issue any warning.
 */
final class switch extends scala.annotation.StaticAnnotation
