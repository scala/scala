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
 * A slice of [[scala.reflect.macros.blackbox.Context the Scala macros context]] that defines shorthands for the
 *  most common `Expr`-creating functions.
 */
trait ExprUtils {
  self: blackbox.Context =>

  /** Shorthand for `Literal(Constant(null))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literalNull: Expr[Null]

  /** Shorthand for `Literal(Constant(()))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literalUnit: Expr[Unit]

  /** Shorthand for `Literal(Constant(true))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literalTrue: Expr[Boolean]

  /** Shorthand for `Literal(Constant(false))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literalFalse: Expr[Boolean]

  /** Shorthand for `Literal(Constant(x: Boolean))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Boolean): Expr[Boolean]

  /** Shorthand for `Literal(Constant(x: Byte))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Byte): Expr[Byte]

  /** Shorthand for `Literal(Constant(x: Short))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Short): Expr[Short]

  /** Shorthand for `Literal(Constant(x: Int))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Int): Expr[Int]

  /** Shorthand for `Literal(Constant(x: Long))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Long): Expr[Long]

  /** Shorthand for `Literal(Constant(x: Float))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Float): Expr[Float]

  /** Shorthand for `Literal(Constant(x: Double))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Double): Expr[Double]

  /** Shorthand for `Literal(Constant(x: String))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: String): Expr[String]

  /** Shorthand for `Literal(Constant(x: Char))` in the underlying `universe`. */
  @deprecated("use quasiquotes instead", "2.11.0")
  def literal(x: Char): Expr[Char]
}
