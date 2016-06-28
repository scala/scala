/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import util.{Pretty, FreqMap, Buildable, ConsoleReporter}
import scala.annotation.tailrec

trait Prop {

  import Prop.{Result, Proof, True, False, Exception, Undecided,
    provedToTrue, secure, mergeRes}
  import Gen.Parameters

  def apply(prms: Parameters): Result

  def map(f: Result => Result): Prop = Prop(prms => f(this(prms)))

  def flatMap(f: Result => Prop): Prop = Prop(prms => f(this(prms))(prms))

  // TODO In 1.12.0, make p call-by-name, and remove the calls to secure()
  // in the methods that use combine()
  def combine(p: Prop)(f: (Result, Result) => Result) =
    for(r1 <- this; r2 <- p) yield f(r1,r2)

  /** Convenience method that checks this property with the given parameters
   *  and reports the result on the console. */
  def check(prms: Test.Parameters): Unit = Test.check(
    if(prms.testCallback.isInstanceOf[ConsoleReporter]) prms
    else prms.withTestCallback(prms.testCallback.chain(ConsoleReporter(1))),
    this
  )

  /** Convenience method that checks this property and reports the
   *  result on the console. The default test parameters
   *  ([[Test.Parameters.default]]) are used for the check. */
  def check: Unit = check(Test.Parameters.default)

  /** Convenience method that checks this property and reports the result
   *  on the console. The provided argument should be a function that takes
   *  the default test parameters ([[Test.Parameters.default]])
   *  as input and outputs a modified [[Test.Parameters]] instance that
   *  Example use:
   *
   *  {{{
   *  p.check(_.withMinSuccessfulTests(500))

   *  p.check { _.
   *    withMinSuccessfulTests(80000).
   *    withWorkers(4)
   *  }
   *  }}}
   */
  def check(paramFun: Test.Parameters => Test.Parameters): Unit = check(
    paramFun(Test.Parameters.default)
  )

  /** Convenience method that checks this property with specified minimal
   *  number of successful test and the given testing parameters, and
   *  reports the result on the console. If you need to get the results
   *  from the test use the `check` methods in [[org.scalacheck.Test]]
   *  instead. */
  @deprecated("Use check(prms.withMinSuccessfulTests(n)) instead", "1.11.2")
  def check(minSuccessfulTests: Int, prms: Test.Parameters): Unit = check(
    prms.withMinSuccessfulTests(minSuccessfulTests)
  )

  /** Convenience method that checks this property with specified minimal
   *  number of successful test and reports the result on the console.
   *  If you need to get the results from the test use
   *  the `check` methods in [[org.scalacheck.Test]] instead. */
  @deprecated("Use check(_.withMinSuccessfulTests(n)) instead", "1.11.2")
  def check(minSuccessfulTests: Int): Unit = check(
    _.withMinSuccessfulTests(minSuccessfulTests)
  )

  /** The logic for main, separated out to make it easier to
   *  avoid System.exit calls.  Returns exit code.
   */
  def mainRunner(args: Array[String]): Int = {
    Test.parseParams(args) match {
      case Some(params) =>
        if (Test.check(params, this).passed) 0
        else 1
      case None =>
        println("Incorrect options")
        -1
    }
  }

  /** Whether main should call System.exit with an exit code.
   *  Defaults to true; override to change. */
  def mainCallsExit = true

  /** Convenience method that makes it possible to use this property
   *  as an application that checks itself on execution */
  def main(args: Array[String]): Unit = {
    val code = mainRunner(args)
    if (mainCallsExit && code != 0)
      System exit code
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate false.  */
  def &&(p: => Prop) = combine(secure(p))(_ && _)

  /** Returns a new property that holds if either this
   *  or the given property (or both) hold.  */
  def ||(p: => Prop) = combine(secure(p))(_ || _)

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate the same result
   *  as the other property.  */
  def ++(p: => Prop): Prop = combine(secure(p))(_ ++ _)

  /** Combines two properties through implication */
  def ==>(p: => Prop): Prop = flatMap { r1 =>
    if(r1.proved) p map { r2 => mergeRes(r1,r2,r2.status) }
    else if(!r1.success) Prop(r1.copy(status = Undecided))
    else p map { r2 => provedToTrue(mergeRes(r1,r2,r2.status)) }
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property generates a result with the exact
   *  same status. Note that this means that if one of the properties is
   *  proved, and the other one passed, then the resulting property
   *  will fail. */
  def ==(p: => Prop) = this.flatMap { r1 =>
    p.map { r2 =>
      mergeRes(r1, r2, if(r1.status == r2.status) True else False)
    }
  }

  override def toString = "Prop"

  /** Put a label on the property to make test reports clearer */
  def label(l: String) = map(_.label(l))

  /** Put a label on the property to make test reports clearer */
  def :|(l: String) = label(l)

  /** Put a label on the property to make test reports clearer */
  def |:(l: String) = label(l)

  /** Put a label on the property to make test reports clearer */
  def :|(l: Symbol) = label(l.toString.drop(1))

  /** Put a label on the property to make test reports clearer */
  def |:(l: Symbol) = label(l.toString.drop(1))

}

object Prop {

  import Gen.{value, fail, frequency, oneOf, Parameters}
  import Arbitrary.{arbitrary}
  import Shrink.{shrink}

  // Types

  /** A property argument */
  case class Arg[+T](
    label: String,
    arg: T,
    shrinks: Int,
    origArg: T,
    prettyArg: Pretty,
    prettyOrigArg: Pretty
  )

  object Result {
    @deprecated("Will be removed in 1.12.0", "1.11.2")
    def apply(st: Status): Result = Result(status = st)
    @deprecated("Will be removed in 1.12.0", "1.11.2")
    def merge(x: Result, y: Result, status: Status) = mergeRes(x,y,status)
  }

  private[scalacheck] def mergeRes(x: Result, y: Result, st: Status) = Result(
    status = st,
    args = x.args ++ y.args,
    collected = x.collected ++ y.collected,
    labels = x.labels ++ y.labels
  )

  /** The result of evaluating a property */
  case class Result(
    status: Status,
    args: List[Arg[Any]] = Nil,
    collected: Set[Any] = Set.empty,
    labels: Set[String] = Set.empty
  ) {
    def success = status match {
      case True => true
      case Proof => true
      case _ => false
    }

    def failure = status match {
      case False => true
      case Exception(_) => true
      case _ => false
    }

    def proved = status == Proof

    def addArg(a: Arg[Any]) = copy(args = a::args)

    def collect(x: Any) = copy(collected = collected+x)

    def label(l: String) = copy(labels = labels+l)

    def &&(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,_) => this
      case (_,False) => r

      case (Undecided,_) => this
      case (_,Undecided) => r

      case (_,Proof) => mergeRes(this, r, this.status)
      case (Proof,_) => mergeRes(this, r, r.status)

      case (True,True) => mergeRes(this, r, True)
    }

    def ||(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,False) => mergeRes(this, r, False)
      case (False,_) => r
      case (_,False) => this

      case (Proof,_) => this
      case (_,Proof) => r

      case (True,_) => this
      case (_,True) => r

      case (Undecided,Undecided) => mergeRes(this, r, Undecided)
    }

    def ++(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (_, Undecided) => this
      case (Undecided, _) => r

      case (_, Proof) => this
      case (Proof, _) => r

      case (_, True) => this
      case (True, _) => r

      case (False, _) => this
      case (_, False) => r
    }

    def ==>(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,_) => mergeRes(this, r, Undecided)

      case (Undecided,_) => this

      case (Proof,_) => mergeRes(this, r, r.status)
      case (True,_) => mergeRes(this, r, r.status)
    }
  }

  sealed trait Status

  /** The property was proved */
  case object Proof extends Status

  /** The property was true */
  case object True extends Status

  /** The property was false */
  case object False extends Status

  /** The property could not be falsified or proved */
  case object Undecided extends Status

  /** Evaluating the property raised an exception */
  sealed case class Exception(e: Throwable) extends Status {
    override def equals(o: Any) = o match {
      case Exception(_) => true
      case _ => false
    }
  }

  /** Create a new property from the given function. */
  def apply(f: Parameters => Result): Prop = new Prop {
    def apply(prms: Parameters) = try f(prms) catch {
      case e: Throwable => Result(status = Exception(e))
    }
  }

  /** Create a property that returns the given result */
  def apply(r: Result): Prop = Prop.apply(prms => r)

  /** Create a property from a boolean value */
  def apply(b: Boolean): Prop = if(b) proved else falsified


  // Implicits

  /** A collection of property operators on `Any` values.
   *  Import [[Prop.AnyOperators]] to make the operators available. */
  class ExtendedAny[T <% Pretty](x: => T) {
    /** See [[Prop.imply]] */
    def imply(f: PartialFunction[T,Prop]) = Prop.imply(x,f)
    /** See [[Prop.iff]] */
    def iff(f: PartialFunction[T,Prop]) = Prop.iff(x,f)
    /** See [[Prop.?=]] */
    def ?=(y: T) = Prop.?=(x, y)
    /** See [[Prop.=?]] */
    def =?(y: T) = Prop.=?(x, y)
  }

  /** A collection of property operators on `Boolean` values.
   *  Import [[Prop.BooleanOperators]] to make the operators available. */
  class ExtendedBoolean(b: => Boolean) {
    /** See the documentation for [[org.scalacheck.Prop]] */
    def ==>(p: => Prop) = Prop(b) ==> p
    /** See the documentation for [[org.scalacheck.Prop]] */
    def :|(l: String) = Prop(b) :| l
    /** See the documentation for [[org.scalacheck.Prop]] */
    def |:(l: String) = l |: Prop(b)
    /** See the documentation for [[org.scalacheck.Prop]] */
    def :|(l: Symbol) = Prop(b) :| l
    /** See the documentation for [[org.scalacheck.Prop]] */
    def |:(l: Symbol) = l |: Prop(b)
  }

  /** Implicit method that makes a number of property operators on values of
   * type `Any` available in the current scope.
   * See [[Prop.ExtendedAny]] for documentation on the operators. */
  implicit def AnyOperators[T <% Pretty](x: => T) = new ExtendedAny[T](x)

  /** Implicit method that makes a number of property operators on boolean
   * values available in the current scope. See [[Prop.ExtendedBoolean]] for
   * documentation on the operators. */
  implicit def BooleanOperators(b: => Boolean) = new ExtendedBoolean(b)

  /** Implicit conversion of Boolean values to Prop values. */
  implicit def propBoolean(b: Boolean): Prop = Prop(b)


  // Private support functions

  private def provedToTrue(r: Result) = r.status match {
    case Proof => r.copy(status = True)
    case _ => r
  }


  // Property combinators

  /** A property that never is proved or falsified */
  lazy val undecided = Prop(Result(status = Undecided))

  /** A property that always is false */
  lazy val falsified = Prop(Result(status = False))

  /** A property that always is proved */
  lazy val proved = Prop(Result(status = Proof))

  /** A property that always is passed */
  lazy val passed = Prop(Result(status = True))

  /** A property that denotes an exception */
  def exception(e: Throwable): Prop = Prop(Result(status = Exception(e)))

  /** A property that denotes an exception */
  lazy val exception: Prop = exception(null)

  /** Create a property that compares to values. If the values aren't equal,
   * the property will fail and report that first value doesn't match the
   * expected (second) value. */
  def ?=[T](x: T, y: T)(implicit pp: T => Pretty): Prop =
    if(x == y) proved else falsified :| {
      val exp = Pretty.pretty[T](y, Pretty.Params(0))
      val act = Pretty.pretty[T](x, Pretty.Params(0))
      "Expected "+exp+" but got "+act
    }

  /** Create a property that compares to values. If the values aren't equal,
   * the property will fail and report that second value doesn't match the
   * expected (first) value. */
  def =?[T](x: T, y: T)(implicit pp: T => Pretty): Prop = ?=(y, x)

  /** A property that depends on the generator size */
  def sizedProp(f: Int => Prop): Prop = Prop { prms =>
    // provedToTrue since if the property is proved for
    // one size, it shouldn't be regarded as proved for
    // all sizes.
    provedToTrue(f(prms.size)(prms))
  }

  /** Implication with several conditions */
  def imply[T](x: T, f: PartialFunction[T,Prop]): Prop = secure {
    if(f.isDefinedAt(x)) f(x) else undecided
  }

  /** Property holds only if the given partial function is defined at
   *  `x`, and returns a property that holds */
  def iff[T](x: T, f: PartialFunction[T,Prop]): Prop = secure {
    if(f.isDefinedAt(x)) f(x) else falsified
  }

  /** Combines properties into one, which is true if and only if all the
   *  properties are true */
  def all(ps: Prop*) = if(ps.isEmpty) proved else Prop(prms =>
    ps.map(p => p(prms)).reduceLeft(_ && _)
  )

  /** Combines properties into one, which is true if at least one of the
   *  properties is true */
  def atLeastOne(ps: Prop*) = if(ps.isEmpty) falsified else Prop(prms =>
    ps.map(p => p(prms)).reduceLeft(_ || _)
  )

  /** A property that holds if at least one of the given generators
   *  fails generating a value */
  def someFailing[T](gs: Seq[Gen[T]]) = atLeastOne(gs.map(_ == fail):_*)

  /** A property that holds iff none of the given generators
   *  fails generating a value */
  def noneFailing[T](gs: Seq[Gen[T]]) = all(gs.map(_ !== fail):_*)

  /** Returns true if the given statement throws an exception
   *  of the specified type */
  def throws[T <: Throwable](c: Class[T])(x: => Any): Boolean =
    try { x; false } catch { case e if c.isInstance(e) => true }

  /** Collect data for presentation in test report */
  def collect[T, P <% Prop](f: T => P): T => Prop = t => Prop { prms =>
    val prop = f(t)
    prop(prms).collect(t)
  }

  /** Collect data for presentation in test report */
  def collect[T](t: T)(prop: Prop) = Prop { prms =>
    prop(prms).collect(t)
  }

  /** Collect data for presentation in test report */
  def classify(c: => Boolean, ifTrue: Any)(prop: Prop): Prop =
    if(c) collect(ifTrue)(prop) else collect(())(prop)

  /** Collect data for presentation in test report */
  def classify(c: => Boolean, ifTrue: Any, ifFalse: Any)(prop: Prop): Prop =
    if(c) collect(ifTrue)(prop) else collect(ifFalse)(prop)

  /** Wraps and protects a property */
  def secure[P <% Prop](p: => P): Prop =
    try (p: Prop) catch { case e: Throwable => exception(e) }

  /** Existential quantifier for an explicit generator. */
  def exists[A,P](f: A => P)(implicit
    pv: P => Prop,
    pp: A => Pretty,
    aa: Arbitrary[A]
  ): Prop = exists(aa.arbitrary)(f)

  /** Existential quantifier for an explicit generator. */
  def exists[A,P](g: Gen[A])(f: A => P)(implicit
    pv: P => Prop,
    pp: A => Pretty
  ): Prop = Prop { prms =>
    val gr = g.doApply(prms)
    gr.retrieve match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        val labels = gr.labels.mkString(",")
        val r = p(prms).addArg(Arg(labels,x,0,x,pp(x),pp(x)))
        r.status match {
          case True => r.copy(status = Proof)
          case False => r.copy(status = Undecided)
          case _ => r
        }
    }
  }

  /** Universal quantifier for an explicit generator. Does not shrink failed
   *  test cases. */
  def forAllNoShrink[T1,P](
    g1: Gen[T1])(
    f: T1 => P)(implicit
    pv: P => Prop,
    pp1: T1 => Pretty
  ): Prop = Prop { prms =>
    val gr = g1.doApply(prms)
    gr.retrieve match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        val labels = gr.labels.mkString(",")
        provedToTrue(p(prms)).addArg(Arg(labels,x,0,x,pp1(x),pp1(x)))
    }
  }

  /** Universal quantifier for two explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,P](
    g1: Gen[T1], g2: Gen[T2])(
    f: (T1,T2) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2)(f(t, _:T2)))

  /** Universal quantifier for three explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3])(
    f: (T1,T2,T3) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3)(f(t, _:T2, _:T3)))

  /** Universal quantifier for four explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,T4,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4])(
    f: (T1,T2,T3,T4) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty,
    pp4: T4 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3,g4)(f(t, _:T2, _:T3, _:T4)))

  /** Universal quantifier for five explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,T4,T5,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5])(
    f: (T1,T2,T3,T4,T5) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty,
    pp4: T4 => Pretty,
    pp5: T5 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3,g4,g5)(f(t, _:T2, _:T3, _:T4, _:T5)))

  /** Universal quantifier for six explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,T4,T5,T6,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6])(
    f: (T1,T2,T3,T4,T5,T6) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty,
    pp4: T4 => Pretty,
    pp5: T5 => Pretty,
    pp6: T6 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3,g4,g5,g6)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6)))

  /** Universal quantifier for seven explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,T4,T5,T6,T7,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7])(
    f: (T1,T2,T3,T4,T5,T6,T7) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty,
    pp4: T4 => Pretty,
    pp5: T5 => Pretty,
    pp6: T6 => Pretty,
    pp7: T7 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3,g4,g5,g6,g7)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7)))

  /** Universal quantifier for eight explicit generators.
   *  Does not shrink failed test cases. */
  def forAllNoShrink[T1,T2,T3,T4,T5,T6,T7,T8,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8])(
    f: (T1,T2,T3,T4,T5,T6,T7,T8) => P)(implicit
    p: P => Prop,
    pp1: T1 => Pretty,
    pp2: T2 => Pretty,
    pp3: T3 => Pretty,
    pp4: T4 => Pretty,
    pp5: T5 => Pretty,
    pp6: T6 => Pretty,
    pp7: T7 => Pretty,
    pp8: T8 => Pretty
  ): Prop = forAllNoShrink(g1)(t => forAllNoShrink(g2,g3,g4,g5,g6,g7,g8)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8)))

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,P](
    f: A1 => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,P](
    f: (A1,A2) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1], arbitrary[A2])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,P](
    f: (A1,A2,A3) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,A4,P](
    f: (A1,A2,A3,A4) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], pp4: A4 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3], arbitrary[A4])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,A4,A5,P](
    f: (A1,A2,A3,A4,A5) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], pp5: A5 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3], arbitrary[A4], arbitrary[A5])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,A4,A5,A6,P](
    f: (A1,A2,A3,A4,A5,A6) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], pp6: A6 => Pretty
  ): Prop = forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3], arbitrary[A4], arbitrary[A5], arbitrary[A6])(f)

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,A4,A5,A6,A7,P](
    f: (A1,A2,A3,A4,A5,A6,A7) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], pp6: A6 => Pretty,
    a7: Arbitrary[A7], pp7: A7 => Pretty
  ): Prop = {
    forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3], arbitrary[A4], arbitrary[A5], arbitrary[A6],
      arbitrary[A7])(f)
  }

  /** Converts a function into a universally quantified property */
  def forAllNoShrink[A1,A2,A3,A4,A5,A6,A7,A8,P](
    f: (A1,A2,A3,A4,A5,A6,A7,A8) => P)(implicit
    pv: P => Prop,
    a1: Arbitrary[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], pp6: A6 => Pretty,
    a7: Arbitrary[A7], pp7: A7 => Pretty,
    a8: Arbitrary[A8], pp8: A8 => Pretty
  ): Prop = {
    forAllNoShrink(arbitrary[A1], arbitrary[A2], arbitrary[A3], arbitrary[A4], arbitrary[A5], arbitrary[A6],
      arbitrary[A7], arbitrary[A8])(f)
  }

  /** Universal quantifier for an explicit generator. Shrinks failed arguments
   *  with the given shrink function */
  def forAllShrink[T, P](g: Gen[T],
    shrink: T => Stream[T])(f: T => P
  )(implicit pv: P => Prop, pp: T => Pretty
  ): Prop = Prop { prms =>

    val gr = g.doApply(prms)
    val labels = gr.labels.mkString(",")

    def result(x: T) = {
      val p = secure(pv(f(x)))
      provedToTrue(p(prms))
    }

    /** Returns the first failed result in Left or success in Right */
    def getFirstFailure(xs: Stream[T]): Either[(T,Result),(T,Result)] = {
      assert(!xs.isEmpty, "Stream cannot be empty")
      val results = xs.map(x => (x, result(x)))
      results.dropWhile(!_._2.failure).headOption match {
        case None => Right(results.head)
        case Some(xr) => Left(xr)
      }
    }

    def shrinker(x: T, r: Result, shrinks: Int, orig: T): Result = {
      val xs = shrink(x).filter(gr.sieve)
      val res = r.addArg(Arg(labels,x,shrinks,orig,pp(x),pp(orig)))
      if(xs.isEmpty) res else getFirstFailure(xs) match {
        case Right((x2,r2)) => res
        case Left((x2,r2)) => shrinker(x2, replOrig(r,r2), shrinks+1, orig)
      }
    }

    def replOrig(r0: Result, r1: Result) = (r0.args,r1.args) match {
      case (a0::_,a1::as) =>
        r1.copy(
          args = a1.copy(
            origArg = a0.origArg,
            prettyOrigArg = a0.prettyOrigArg
          ) :: as
        )
      case _ => r1
    }

    gr.retrieve match {
      case None => undecided(prms)
      case Some(x) =>
        val r = result(x)
        if (!r.failure) r.addArg(Arg(labels,x,0,x,pp(x),pp(x)))
        else shrinker(x,r,0,x)
    }

  }

  /** Universal quantifier for an explicit generator. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,P](
    g1: Gen[T1])(
    f: T1 => P)(implicit
    p: P => Prop,
    s1: Shrink[T1],
    pp1: T1 => Pretty
  ): Prop = forAllShrink[T1,P](g1, shrink[T1])(f)

  /** Universal quantifier for two explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,P](
    g1: Gen[T1], g2: Gen[T2])(
    f: (T1,T2) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2)(f(t, _:T2)))

  /** Universal quantifier for three explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3])(
    f: (T1,T2,T3) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3)(f(t, _:T2, _:T3)))

  /** Universal quantifier for four explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,T4,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4])(
    f: (T1,T2,T3,T4) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty,
    s4: Shrink[T4], pp4: T4 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3,g4)(f(t, _:T2, _:T3, _:T4)))

  /** Universal quantifier for five explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,T4,T5,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5])(
    f: (T1,T2,T3,T4,T5) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty,
    s4: Shrink[T4], pp4: T4 => Pretty,
    s5: Shrink[T5], pp5: T5 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3,g4,g5)(f(t, _:T2, _:T3, _:T4, _:T5)))

  /** Universal quantifier for six explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,T4,T5,T6,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6])(
    f: (T1,T2,T3,T4,T5,T6) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty,
    s4: Shrink[T4], pp4: T4 => Pretty,
    s5: Shrink[T5], pp5: T5 => Pretty,
    s6: Shrink[T6], pp6: T6 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3,g4,g5,g6)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6)))

  /** Universal quantifier for seven explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,T4,T5,T6,T7,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7])(
    f: (T1,T2,T3,T4,T5,T6,T7) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty,
    s4: Shrink[T4], pp4: T4 => Pretty,
    s5: Shrink[T5], pp5: T5 => Pretty,
    s6: Shrink[T6], pp6: T6 => Pretty,
    s7: Shrink[T7], pp7: T7 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3,g4,g5,g6,g7)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7)))

  /** Universal quantifier for eight explicit generators. Shrinks failed arguments
   *  with the default shrink function for the type */
  def forAll[T1,T2,T3,T4,T5,T6,T7,T8,P](
    g1: Gen[T1], g2: Gen[T2], g3: Gen[T3], g4: Gen[T4], g5: Gen[T5], g6: Gen[T6], g7: Gen[T7], g8: Gen[T8])(
    f: (T1,T2,T3,T4,T5,T6,T7,T8) => P)(implicit
    p: P => Prop,
    s1: Shrink[T1], pp1: T1 => Pretty,
    s2: Shrink[T2], pp2: T2 => Pretty,
    s3: Shrink[T3], pp3: T3 => Pretty,
    s4: Shrink[T4], pp4: T4 => Pretty,
    s5: Shrink[T5], pp5: T5 => Pretty,
    s6: Shrink[T6], pp6: T6 => Pretty,
    s7: Shrink[T7], pp7: T7 => Pretty,
    s8: Shrink[T8], pp8: T8 => Pretty
  ): Prop = forAll(g1)(t => forAll(g2,g3,g4,g5,g6,g7,g8)(f(t, _:T2, _:T3, _:T4, _:T5, _:T6, _:T7, _:T8)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,P] (
    f: A1 => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty
  ): Prop = forAllShrink(arbitrary[A1],shrink[A1])(f andThen p)

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,P] (
    f: (A1,A2) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,P] (
    f: (A1,A2,A3) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,P] (
    f: (A1,A2,A3,A4) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,P] (
    f: (A1,A2,A3,A4,A5) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,P] (
    f: (A1,A2,A3,A4,A5,A6) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], s6: Shrink[A6], pp6: A6 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,A7,P] (
    f: (A1,A2,A3,A4,A5,A6,A7) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], s6: Shrink[A6], pp6: A6 => Pretty,
    a7: Arbitrary[A7], s7: Shrink[A7], pp7: A7 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7)))

  /** Converts a function into a universally quantified property */
  def forAll[A1,A2,A3,A4,A5,A6,A7,A8,P] (
    f: (A1,A2,A3,A4,A5,A6,A7,A8) => P)(implicit
    p: P => Prop,
    a1: Arbitrary[A1], s1: Shrink[A1], pp1: A1 => Pretty,
    a2: Arbitrary[A2], s2: Shrink[A2], pp2: A2 => Pretty,
    a3: Arbitrary[A3], s3: Shrink[A3], pp3: A3 => Pretty,
    a4: Arbitrary[A4], s4: Shrink[A4], pp4: A4 => Pretty,
    a5: Arbitrary[A5], s5: Shrink[A5], pp5: A5 => Pretty,
    a6: Arbitrary[A6], s6: Shrink[A6], pp6: A6 => Pretty,
    a7: Arbitrary[A7], s7: Shrink[A7], pp7: A7 => Pretty,
    a8: Arbitrary[A8], s8: Shrink[A8], pp8: A8 => Pretty
  ): Prop = forAll((a: A1) => forAll(f(a, _:A2, _:A3, _:A4, _:A5, _:A6, _:A7, _:A8)))

  /** Ensures that the property expression passed in completes within the given
   *  space of time. */
  def within(maximumMs: Long)(wrappedProp: => Prop): Prop = new Prop {
    @tailrec private def attempt(prms: Parameters, endTime: Long): Result = {
      val result = wrappedProp.apply(prms)
      if (System.currentTimeMillis > endTime) {
        (if(result.failure) result else Result(status = False)).label("Timeout")
      } else {
        if (result.success) result
        else attempt(prms, endTime)
      }
    }
    def apply(prms: Parameters) = attempt(prms, System.currentTimeMillis + maximumMs)
  }
}
