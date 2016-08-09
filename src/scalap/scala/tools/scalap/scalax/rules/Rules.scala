// -----------------------------------------------------------------------------
//
//  Scalax - The Scala Community Library
//  Copyright (c) 2005-8 The Scalax Project. All rights reserved.
//
//  The primary distribution site is http://scalax.scalaforge.org/
//
//  This software is released under the terms of the Revised BSD License.
//  There is NO WARRANTY.  See the file LICENSE for the full text.
//
// -----------------------------------------------------------------------------

package scala.tools.scalap
package scalax
package rules

import language.postfixOps

trait Name {
  def name: String
  override def toString = name
}

/** A factory for rules.
  *
  * @author Andrew Foggin
  *
  * Inspired by the Scala parser combinator.
  */
trait Rules {

  import scala.language.implicitConversions
  implicit def rule[In, Out, A, X](f: In => Result[Out, A, X]): Rule[In, Out, A, X] = new DefaultRule(f)
  implicit def inRule[In, Out, A, X](rule: Rule[In, Out, A, X]): InRule[In, Out, A, X] = new InRule(rule)
  implicit def seqRule[In, A, X](rule: Rule[In, In, A, X]): SeqRule[In, A, X] = new SeqRule(rule)

  trait FromRule[In] {
    def apply[Out, A, X](f: In => Result[Out, A, X]): Rule[In, Out, A, X]
  }

  def from[In] = new FromRule[In] {
    def apply[Out, A, X](f: In => Result[Out, A, X]) = rule(f)
  }

  def state[s] = new StateRules {
    type S = s
    val factory = Rules.this
  }

  def success[Out, A](out: Out, a: A) = rule { in: Any => Success(out, a) }

  def failure = rule { in: Any => Failure }

  def error[In] = rule { in: In => Error(in) }
  def error[X](err: X) = rule { in: Any => Error(err) }

  def oneOf[In, Out, A, X](rules: Rule[In, Out, A, X] *): Rule[In, Out, A, X] = new Choice[In, Out, A, X] {
    val factory = Rules.this
    val choices = rules.toList
  }

  def ruleWithName[In, Out, A, X](_name: String, f: In => Result[Out, A, X]): Rule[In, Out, A, X] with Name =
    new DefaultRule(f) with Name {
      val name = _name
    }

  class DefaultRule[In, Out, A, X](f: In => Result[Out, A, X]) extends Rule[In, Out, A, X] {
    val factory = Rules.this
    def apply(in: In) = f(in)
  }

 /** Converts a rule into a function that throws an Exception on failure. */
  def expect[In, Out, A, Any](rule: Rule[In, Out, A, Any]): In => A = (in) => rule(in) match {
    case Success(_, a) => a
    case Failure => throw new ScalaSigParserError("Unexpected failure")
    case Error(x) => throw new ScalaSigParserError("Unexpected error: " + x)
  }
}

/** A factory for rules that apply to a particular context.
  *
  * @tparam S the context to which rules apply.
  *
  * @author Andrew Foggin
  *
  * Inspired by the Scala parser combinator.
  */
trait StateRules {
  type S
  type Rule[+A, +X] = rules.Rule[S, S, A, X]

  val factory: Rules
  import factory._

  def apply[A, X](f: S => Result[S, A, X]) = rule(f)

  def unit[A](a: => A) = apply { s => Success(s, a) }
  def read[A](f: S => A) = apply { s => Success(s, f(s)) }

  def get = apply { s => Success(s, s) }
  def set(s: => S) = apply { oldS => Success(s, oldS) }

  def update(f: S => S) = apply { s => Success(s, f(s)) }

  def nil = unit(Nil)
  def none = unit(None)

  /** Create a rule that identities if f(in) is true. */
  def cond(f: S => Boolean) = get filter f

  /** Create a rule that succeeds if all of the given rules succeed.
      @param rules the rules to apply in sequence.
  */
  def allOf[A, X](rules: Seq[Rule[A, X]]) = {
    def rep(in: S, rules: List[Rule[A, X]], results: List[A]): Result[S, List[A], X] = {
      rules match {
        case Nil => Success(in, results.reverse)
        case rule::tl => rule(in) match {
          case Failure => Failure
          case Error(x) => Error(x)
          case Success(out, v) => rep(out, tl, v::results)
        }
      }
    }
    in: S => rep(in, rules.toList, Nil)
  }


  /** Create a rule that succeeds with a list of all the provided rules that succeed.
      @param rules the rules to apply in sequence.
  */
  def anyOf[A, X](rules: Seq[Rule[A, X]]) = allOf(rules.map(_ ?)) ^^ { opts => opts.flatMap(x => x) }

  /** Repeatedly apply a rule from initial value until finished condition is met. */
  def repeatUntil[T, X](rule: Rule[T => T, X])(finished: T => Boolean)(initial: T) = apply {
    // more compact using HoF but written this way so it's tail-recursive
    def rep(in: S, t: T): Result[S, T, X] = {
      if (finished(t)) Success(in, t)
      else rule(in) match {
        case Success(out, f) => rep(out, f(t)) // SI-5189 f.asInstanceOf[T => T]
        case Failure => Failure
        case Error(x) => Error(x)
      }
    }
    in => rep(in, initial)
  }


}

trait RulesWithState extends Rules with StateRules {
  val factory = this
}
