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

package scala
package reflect
package macros

/**
 * <span class="badge badge-red" style="float: right;">EXPERIMENTAL</span>
 *
 *  A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that
 *  exposes functions to parse strings with Scala code into trees.
 */
trait Parsers {
  self: blackbox.Context =>

  /** Parses a string with a Scala expression into an abstract syntax tree.
   *  Only works for expressions, i.e. parsing a package declaration will fail.
   *  @throws scala.reflect.macros.ParseException
   */
  def parse(code: String): Tree
}

/** Indicates an error during [[scala.reflect.macros.Parsers#parse]].
 */
case class ParseException(pos: scala.reflect.api.Position, msg: String) extends Exception(msg)
