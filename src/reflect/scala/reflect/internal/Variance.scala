/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala
package reflect
package internal

import Variance._

/** Variances form a lattice:
 *
 *            - Covariant -
 *           /             \
 *  Invariant               Bivariant
 *           \             /
 *            Contravariant
 *
 *  The variance of a symbol within a type is calculated based on variance
 *  annotations, e.g. +A or -A, and the positions of the types in which the
 *  symbol appears. The actual mechanics are beyond the scope of this
 *  comment, but the essential operations on a Variance are:
 *
 *  '&'  - like bitwise AND. Unless all inputs have compatible variance,
 *  folding them across & will be invariant.
 *  '*'  - like multiplication across { -1, 0, 1 } with contravariance as -1.
 *  flip - if contravariant or covariant, flip to the other; otherwise leave unchanged.
 *  cut  - if bivariant, remain bivariant; otherwise become invariant.
 *
 *  There is an important distinction between "isPositive" and "isCovariant".
 *  The former is true for both Covariant and Bivariant, but the latter is true
 *  only for Covariant.
 */
final class Variance private (val flags: Int) extends AnyVal {
  def isBivariant     = flags == 2
  def isCovariant     = flags == 1    // excludes bivariant
  def isInvariant     = flags == 0
  def isContravariant = flags == -1   // excludes bivariant
  def isPositive      = flags > 0     // covariant or bivariant

  def &(other: Variance): Variance = (
    if (this == other) this
    else if (this.isBivariant) other
    else if (other.isBivariant) this
    else Invariant
  )

  def *(other: Variance): Variance = (
    if (other.isPositive) this
    else if (other.isContravariant) this.flip
    else this.cut
  )

  /** Flip between covariant and contravariant. I chose not to use unary_- because it doesn't stand out enough. */
  def flip = if (isCovariant) Contravariant else if (isContravariant) Covariant else this

  /** Map everything below bivariant to invariant. */
  def cut  = if (isBivariant) this else Invariant

  /** The symbolic annotation used to indicate the given kind of variance. */
  def symbolicString = (
    if (isCovariant) "+"
    else if (isContravariant) "-"
    else ""
  )

  override def toString = (
    if (isContravariant) "contravariant"
    else if (isCovariant) "covariant"
    else if (isInvariant) "invariant"
    else "" // noisy to print bivariant on everything without type parameters
  )
}

object Variance {
  implicit class SbtCompat(val v: Variance) {
    def < (other: Int) = v.flags < other
    def > (other: Int) = v.flags > other
  }

  def fold(variances: List[Variance]): Variance = (
    if (variances.isEmpty) Bivariant
    else variances reduceLeft (_ & _)
  )
  val Bivariant     = new Variance(2)
  val Covariant     = new Variance(1)
  val Contravariant = new Variance(-1)
  val Invariant     = new Variance(0)
}
