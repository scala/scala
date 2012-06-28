/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import util.{FreqMap,Buildable}
import scala.collection._
import scala.annotation.tailrec

/** A property is a generator that generates a property result */
trait Prop {

  import Prop.{Result,Params,Proof,True,False,Exception,Undecided,provedToTrue}
  import Test.cmdLineParser.{Success, NoSuccess}
  import Result.merge

  def apply(prms: Params): Result

  def map(f: Result => Result): Prop = Prop(prms => f(this(prms)))

  def flatMap(f: Result => Prop): Prop = Prop(prms => f(this(prms))(prms))

  def combine(p: Prop)(f: (Result, Result) => Result) =
    for(r1 <- this; r2 <- p) yield f(r1,r2)

  /** Convenience method that checks this property with the given parameters
   *  and reports the result on the console. If you need to get the results
   *  from the test use the <code>check</code> methods in <code>Test</code>
   *  instead. */
  def check(prms: Test.Params): Unit = Test.check(
    prms copy (testCallback = ConsoleReporter(1) chain prms.testCallback), this
  )

  /** Convenience method that checks this property and reports the
   *  result on the console. If you need to get the results from the test use
   *  the <code>check</code> methods in <code>Test</code> instead. */
  def check: Unit = check(Test.Params())

  /** The logic for main, separated out to make it easier to
   *  avoid System.exit calls.  Returns exit code.
   */
  def mainRunner(args: Array[String]): Int = {
    Test.cmdLineParser.parseParams(args) match {
      case Success(params, _) =>
        if (Test.check(params, this).passed) 0
        else 1
      case e: NoSuccess =>
        println("Incorrect options:"+"\n"+e+"\n")
        Test.cmdLineParser.printHelp
        -1
    }
  }

  /** Whether main should call System.exit with an exit code.
   *  Defaults to true; override to change.
   */
  def mainCallsExit = false

  /** Convenience method that makes it possible to use this property
   *  as an application that checks itself on execution */
  def main(args: Array[String]): Unit = {
    val code = mainRunner(args)
    if (mainCallsExit)
      System exit code
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate false.  */
  def &&(p: Prop) = combine(p)(_ && _)

  /** Returns a new property that holds if either this
   *  or the given property (or both) hold.  */
  def ||(p: Prop) = combine(p)(_ || _)

  /** Returns a new property that holds if and only if both this
   *  and the given property hold. If one of the properties doesn't
   *  generate a result, the new property will generate the same result
   *  as the other property.  */
  def ++(p: Prop): Prop = combine(p)(_ ++ _)

  /** Combines two properties through implication */
  def ==>(p: => Prop): Prop = flatMap { r1 =>
    if(r1.proved) p map { r2 => merge(r1,r2,r2.status) }
    else if(r1.success) p map { r2 => provedToTrue(merge(r1,r2,r2.status)) }
    else Prop(r1.copy(status = Undecided))
  }

  /** Returns a new property that holds if and only if both this
   *  and the given property generates a result with the exact
   *  same status. Note that this means that if one of the properties is
   *  proved, and the other one passed, then the resulting property
   *  will fail. */
  def ==(p: Prop) = this.flatMap { r1 =>
    p.map { r2 =>
      Result.merge(r1, r2, if(r1.status == r2.status) True else False)
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

  import Gen.{value, fail, frequency, oneOf}
  import Arbitrary._
  import Shrink._


  // Types

  type Args = List[Arg[Any]]
  type FM = FreqMap[immutable.Set[Any]]

  /** Property parameters */
  case class Params(val genPrms: Gen.Params, val freqMap: FM)

  object Result {
    def apply(st: Status) = new Result(
      st,
      Nil,
      immutable.Set.empty[Any],
      immutable.Set.empty[String]
    )

    def merge(x: Result, y: Result, status: Status) = new Result(
      status,
      x.args ++ y.args,
      (x.collected.asInstanceOf[Set[AnyRef]] ++ y.collected).asInstanceOf[immutable.Set[Any]],
      x.labels ++ y.labels
    )
  }

  /** The result of evaluating a property */
  case class Result(
    status: Status,
    args: Args,
    collected: immutable.Set[Any],
    labels: immutable.Set[String]
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

    import Result.merge

    def &&(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,_) => this
      case (_,False) => r

      case (Undecided,_) => this
      case (_,Undecided) => r

      case (_,Proof) => merge(this, r, this.status)
      case (Proof,_) => merge(this, r, r.status)

      case (True,True) => merge(this, r, True)
    }

    def ||(r: Result) = (this.status, r.status) match {
      case (Exception(_),_) => this
      case (_,Exception(_)) => r

      case (False,False) => merge(this, r, False)
      case (False,_) => r
      case (_,False) => this

      case (Proof,_) => this
      case (_,Proof) => r

      case (True,_) => this
      case (_,True) => r

      case (Undecided,Undecided) => merge(this, r, Undecided)
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

      case (False,_) => merge(this, r, Undecided)

      case (Undecided,_) => this

      case (Proof,_) => merge(this, r, r.status)
      case (True,_) => merge(this, r, r.status)
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

  def apply(f: Params => Result): Prop = new Prop {
    def apply(prms: Params) = f(prms)
  }

  def apply(r: Result): Prop = Prop(prms => r)


  // Implicit defs

  class ExtendedAny[T <% Pretty](x: => T) {
    def imply(f: PartialFunction[T,Prop]) = Prop.imply(x,f)
    def iff(f: PartialFunction[T,Prop]) = Prop.iff(x,f)
    def throws[U <: Throwable](c: Class[U]) = Prop.throws(x, c)
    def ?=(y: T) = Prop.?=(x, y)
    def =?(y: T) = Prop.=?(x, y)
  }

  implicit def extendedAny[T <% Pretty](x: => T) = new ExtendedAny[T](x)

  implicit def propBoolean(b: Boolean): Prop = if(b) proved else falsified


  // Private support functions

  private def provedToTrue(r: Result) = r.status match {
    case Proof => new Result(True, r.args, r.collected, r.labels)
    case _ => r
  }


  // Property combinators

  /** A property that never is proved or falsified */
  lazy val undecided = Prop(Result(Undecided))

  /** A property that always is false */
  lazy val falsified = Prop(Result(False))

  /** A property that always is proved */
  lazy val proved = Prop(Result(Proof))

  /** A property that always is passed */
  lazy val passed = Prop(Result(True))

  /** A property that denotes an exception */
  def exception(e: Throwable): Prop = Prop(Result(Exception(e)))

  /** A property that denotes an exception */
  lazy val exception: Prop = exception(null)

  def ?=[T](x: T, y: T)(implicit pp: T => Pretty): Prop =
    if(x == y) proved else falsified :| {
      val exp = Pretty.pretty[T](y, Pretty.Params(0))
      val act = Pretty.pretty[T](x, Pretty.Params(0))
      "Expected "+exp+" but got "+act
    }

  def =?[T](x: T, y: T)(implicit pp: T => Pretty): Prop = ?=(y, x)

  /** A property that depends on the generator size */
  def sizedProp(f: Int => Prop): Prop = Prop { prms =>
    // provedToTrue since if the property is proved for
    // one size, it shouldn't be regarded as proved for
    // all sizes.
    provedToTrue(f(prms.genPrms.size)(prms))
  }

  /** Implication with several conditions */
  def imply[T](x: T, f: PartialFunction[T,Prop]): Prop =
    secure(if(f.isDefinedAt(x)) f(x) else undecided)

  /** Property holds only if the given partial function is defined at
   *  <code>x</code>, and returns a property that holds */
  def iff[T](x: T, f: PartialFunction[T,Prop]): Prop =
    secure(if(f.isDefinedAt(x)) f(x) else falsified)

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

  /** A property that holds if the given statement throws an exception
   *  of the specified type */
  def throws[T <: Throwable](x: => Any, c: Class[T]) =
    try { x; falsified } catch { case e if c.isInstance(e) => proved }

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
    try { p: Prop } catch { case e => exception(e) }

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
    g(prms.genPrms) match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        val r = p(prms).addArg(Arg(g.label,x,0,x))
        r.status match {
          case True => new Result(Proof, r.args, r.collected, r.labels)
          case False => new Result(Undecided, r.args, r.collected, r.labels)
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
    g1(prms.genPrms) match {
      case None => undecided(prms)
      case Some(x) =>
        val p = secure(f(x))
        provedToTrue(p(prms)).addArg(Arg(g1.label,x,0,x))
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

  /** Universal quantifier for an explicit generator. Shrinks failed arguments
   *  with the given shrink function */
  def forAllShrink[T <% Pretty, P <% Prop](g: Gen[T],
    shrink: T => Stream[T])(f: T => P
  ): Prop = Prop { prms =>

    /** Returns the first failed result in Left or success in Right */
    def getFirstFailure(xs: Stream[T]): Either[(T,Result),(T,Result)] = {
      assert(!xs.isEmpty, "Stream cannot be empty")
      val results = xs.map { x =>
        val p = secure(f(x))
        (x, provedToTrue(p(prms)))
      }
      results.dropWhile(!_._2.failure).headOption match {
        case None => Right(results.head)
        case Some(xr) => Left(xr)
      }
    }

    def shrinker(x: T, r: Result, shrinks: Int, orig: T): Result = {
      val xs = shrink(x)
      val res = r.addArg(Arg(g.label,x,shrinks,orig))
      if(xs.isEmpty) res else getFirstFailure(xs) match {
        case Right(_) => res
        case Left((x2,r2)) => shrinker(x2, r2, shrinks+1, orig)
      }
    }

    g(prms.genPrms) match {
      case None => undecided(prms)
      case Some(x) => getFirstFailure(Stream.cons(x, Stream.empty)) match {
        case Right((x,r)) => r.addArg(Arg(g.label,x,0,x))
        case Left((x,r)) => shrinker(x,r,0,x)
      }
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
  ): Prop = forAllShrink(g1, shrink[T1])(f)

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

  /** Ensures that the property expression passed in completes within the given space of time. */
  def within(maximumMs: Long)(wrappedProp: => Prop): Prop = new Prop {
    @tailrec private def attempt(prms: Params, endTime: Long): Result = {
      val result = wrappedProp.apply(prms)
      if (System.currentTimeMillis > endTime) {
        (if (result.failure) result else Result(False)).label("Timeout")
      } else {
        if (result.success) result
        else attempt(prms, endTime)
      }
    }
    def apply(prms: Params) = attempt(prms, System.currentTimeMillis + maximumMs)
  }
}
