/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2014 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Prop.Arg

object Test {

  import util.{FreqMap, ConsoleReporter}

  /** Test parameters used by the check methods. Default
   *  parameters are defined by [[Test.Parameters.Default]]. */
  trait Parameters {
    /** The minimum number of tests that must succeed for ScalaCheck to
     *  consider a property passed. */
    val minSuccessfulTests: Int

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.minSuccessfulTests]] set to the specified value. */
    def withMinSuccessfulTests(minSuccessfulTests: Int): Parameters = cp(
      minSuccessfulTests = minSuccessfulTests
    )

    /** The starting size given as parameter to the generators. */
    val minSize: Int

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.minSize]] set to the specified value. */
    def withMinSize(minSize: Int): Parameters = cp(
      minSize = minSize
    )

    /** The maximum size given as parameter to the generators. */
    val maxSize: Int

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.maxSize]] set to the specified value. */
    def withMaxSize(maxSize: Int): Parameters = cp(
      maxSize = maxSize
    )

    /** The random number generator used. */
    val rng: scala.util.Random

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.rng]] set to the specified value. */
    def withRng(rng: scala.util.Random): Parameters = cp(
      rng = rng
    )

    /** The number of tests to run in parallel. */
    val workers: Int

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.workers]] set to the specified value. */
    def withWorkers(workers: Int): Parameters = cp(
      workers = workers
    )

    /** A callback that ScalaCheck calls each time a test is executed. */
    val testCallback: TestCallback

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.testCallback]] set to the specified value. */
    def withTestCallback(testCallback: TestCallback): Parameters = cp(
      testCallback = testCallback
    )

    /** The maximum ratio between discarded and passed tests allowed before
     *  ScalaCheck gives up and discards the property. At least
     *  `minSuccesfulTests` will always be run, though. */
    val maxDiscardRatio: Float

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.maxDiscardRatio]] set to the specified value. */
    def withMaxDiscardRatio(maxDiscardRatio: Float): Parameters = cp(
      maxDiscardRatio = maxDiscardRatio
    )

    /** A custom class loader that should be used during test execution. */
    val customClassLoader: Option[ClassLoader]

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.customClassLoader]] set to the specified value. */
    def withCustomClassLoader(customClassLoader: Option[ClassLoader]
    ): Parameters = cp(
      customClassLoader = customClassLoader
    )

    // private since we can't guarantee binary compatibility for this one
    private case class cp(
      minSuccessfulTests: Int = minSuccessfulTests,
      minSize: Int = minSize,
      maxSize: Int = maxSize,
      rng: scala.util.Random = rng,
      workers: Int = workers,
      testCallback: TestCallback = testCallback,
      maxDiscardRatio: Float = maxDiscardRatio,
      customClassLoader: Option[ClassLoader] = customClassLoader
    ) extends Parameters
  }

  /** Test parameters used by the check methods. Default
   *  parameters are defined by [[Test.Parameters.Default]]. */
  object Parameters {
    /** Default test parameters trait. This can be overriden if you need to
     *  tweak the parameters:
     *
     *  {{{
     *  val myParams = new Parameters.Default {
     *    override val minSuccesfulTests = 600
     *    override val maxDiscardRatio = 8
     *  }
     *  }}}
     *
     *  You can also use the withXXX-methods in
     *  [[org.scalacheck.Test.Parameters]] to achieve
     *  the same thing:
     *
     *  {{{
     *  val myParams = Parameters.default
     *    .withMinSuccessfulTests(600)
     *    .withMaxDiscardRatio(8)
     *  }}} */
    trait Default extends Parameters {
      val minSuccessfulTests: Int = 100
      val minSize: Int = 0
      val maxSize: Int = Gen.Parameters.default.size
      val rng: scala.util.Random = Gen.Parameters.default.rng
      val workers: Int = 1
      val testCallback: TestCallback = new TestCallback {}
      val maxDiscardRatio: Float = 5
      val customClassLoader: Option[ClassLoader] = None
    }

    /** Default test parameters instance. */
    val default: Parameters = new Default {}

    /** Verbose console reporter test parameters instance. */
    val defaultVerbose: Parameters = new Default {
      override val testCallback = ConsoleReporter(2)
    }
  }

  /** Test statistics */
  case class Result(
    status: Status,
    succeeded: Int,
    discarded: Int,
    freqMap: FreqMap[Set[Any]],
    time: Long = 0
  ) {
    def passed = status match {
      case Passed => true
      case Proved(_) => true
      case _ => false
    }
  }

  /** Test status */
  sealed trait Status

  /** ScalaCheck found enough cases for which the property holds, so the
   *  property is considered correct. (It is not proved correct, though). */
  case object Passed extends Status

  /** ScalaCheck managed to prove the property correct */
  sealed case class Proved(args: List[Arg[Any]]) extends Status

  /** The property was proved wrong with the given concrete arguments.  */
  sealed case class Failed(args: List[Arg[Any]], labels: Set[String]) extends Status

  /** The property test was exhausted, it wasn't possible to generate enough
   *  concrete arguments satisfying the preconditions to get enough passing
   *  property evaluations. */
  case object Exhausted extends Status

  /** An exception was raised when trying to evaluate the property with the
   *  given concrete arguments. If an exception was raised before or during
   *  argument generation, the argument list will be empty. */
  sealed case class PropException(args: List[Arg[Any]], e: Throwable,
    labels: Set[String]) extends Status

  /** An exception was raised when trying to generate concrete arguments
   *  for evaluating the property.
   *  @deprecated Not used. The type PropException is used for all exceptions.
   */
  @deprecated("Not used. The type PropException is used for all exceptions.", "1.11.2")
  sealed case class GenException(e: Throwable) extends Status

  trait TestCallback { self =>
    /** Called each time a property is evaluated */
    def onPropEval(name: String, threadIdx: Int, succeeded: Int,
      discarded: Int): Unit = ()

    /** Called whenever a property has finished testing */
    def onTestResult(name: String, result: Result): Unit = ()

    def chain(testCallback: TestCallback) = new TestCallback {
      override def onPropEval(name: String, threadIdx: Int,
        succeeded: Int, discarded: Int
      ): Unit = {
        self.onPropEval(name,threadIdx,succeeded,discarded)
        testCallback.onPropEval(name,threadIdx,succeeded,discarded)
      }

      override def onTestResult(name: String, result: Result): Unit = {
        self.onTestResult(name,result)
        testCallback.onTestResult(name,result)
      }
    }
  }

  private def assertParams(prms: Parameters) = {
    import prms._
    if(
      minSuccessfulTests <= 0 ||
      maxDiscardRatio <= 0 ||
      minSize < 0 ||
      maxSize < minSize ||
      workers <= 0
    ) throw new IllegalArgumentException("Invalid test parameters")
  }

  private def secure[T](x: => T): Either[T,Throwable] =
    try { Left(x) } catch { case e: Throwable => Right(e) }

  def parseParams(args: Array[String]): Option[Parameters] = {
    var params = Parameters.default
    args.grouped(2).filter(_.size > 1).map(a => (a(0), a(1))).foreach {
      case ("-workers" | "-w", n) => params = params.withWorkers(n.toInt)
      case ("-minSize" | "-n", n) => params = params.withMinSize(n.toInt)
      case ("-maxSize" | "-x", n) => params = params.withMaxSize(n.toInt)
      case ("-verbosity" | "-v", n) => params = params.withTestCallback(ConsoleReporter(n.toInt))
      case ("-maxDiscardRatio" | "-r", n) => params = params.withMaxDiscardRatio(n.toFloat)
      case ("-minSuccessfulTests" | "-s", n) => params = params.withMinSuccessfulTests(n.toInt)
      case _ =>
    }
    Some(params)
  }

  /** Tests a property with parameters that are calculated by applying
   *  the provided function to [[Test.Parameters.default]].
   *  Example use:
   *
   *  {{{
   *  Test.check(p) { _.
   *    withMinSuccessfulTests(80000).
   *    withWorkers(4)
   *  }
   *  }}}
   */
  def check(p: Prop)(f: Parameters => Parameters): Result =
    check(f(Parameters.default), p)

  /** Tests a property with the given testing parameters, and returns
   *  the test results. */
  def check(params: Parameters, p: Prop): Result = {
    import params._
    import concurrent._

    assertParams(params)
    if(workers > 1) {
      assert(!p.isInstanceOf[Commands], "Commands cannot be checked multi-threaded")
    }

    val iterations = math.ceil(minSuccessfulTests / (workers: Double))
    val sizeStep = (maxSize-minSize) / (iterations*workers)
    var stop = false
    val genPrms = new Gen.Parameters.Default { override val rng = params.rng }
    val tp = java.util.concurrent.Executors.newFixedThreadPool(workers)
    implicit val ec = ExecutionContext.fromExecutor(tp)

    def workerFun(workerIdx: Int): Result = {
      var n = 0  // passed tests
      var d = 0  // discarded tests
      var res: Result = null
      var fm = FreqMap.empty[Set[Any]]
      while(!stop && res == null && n < iterations) {
        val size = (minSize: Double) + (sizeStep * (workerIdx + (workers*(n+d))))
        val propRes = p(genPrms.withSize(size.round.toInt))
        fm = if(propRes.collected.isEmpty) fm else fm + propRes.collected
        propRes.status match {
          case Prop.Undecided =>
            d += 1
            testCallback.onPropEval("", workerIdx, n, d)
            // The below condition is kind of hacky. We have to have
            // some margin, otherwise workers might stop testing too
            // early because they have been exhausted, but the overall
            // test has not.
            if (n+d > minSuccessfulTests && 1+workers*maxDiscardRatio*n < d)
              res = Result(Exhausted, n, d, fm)
          case Prop.True =>
            n += 1
            testCallback.onPropEval("", workerIdx, n, d)
          case Prop.Proof =>
            n += 1
            res = Result(Proved(propRes.args), n, d, fm)
            stop = true
          case Prop.False =>
            res = Result(Failed(propRes.args,propRes.labels), n, d, fm)
            stop = true
          case Prop.Exception(e) =>
            res = Result(PropException(propRes.args,e,propRes.labels), n, d, fm)
            stop = true
        }
      }
      if (res == null) {
        if (maxDiscardRatio*n > d) Result(Passed, n, d, fm)
        else Result(Exhausted, n, d, fm)
      } else res
    }

    def mergeResults(r1: Result, r2: Result): Result = {
      val Result(st1, s1, d1, fm1, _) = r1
      val Result(st2, s2, d2, fm2, _) = r2
      if (st1 != Passed && st1 != Exhausted)
        Result(st1, s1+s2, d1+d2, fm1++fm2, 0)
      else if (st2 != Passed && st2 != Exhausted)
        Result(st2, s1+s2, d1+d2, fm1++fm2, 0)
      else {
        if (s1+s2 >= minSuccessfulTests && maxDiscardRatio*(s1+s2) >= (d1+d2))
          Result(Passed, s1+s2, d1+d2, fm1++fm2, 0)
        else
          Result(Exhausted, s1+s2, d1+d2, fm1++fm2, 0)
      }
    }

    try {
      val start = System.currentTimeMillis
      val r =
        if(workers < 2) workerFun(0)
        else {
          val fs = List.range(0,workers) map (idx => Future {
            params.customClassLoader.map(
              Thread.currentThread.setContextClassLoader(_)
            )
            blocking { workerFun(idx) }
          })
          val zeroRes = Result(Passed,0,0,FreqMap.empty[Set[Any]],0)
          val res = Future.fold(fs)(zeroRes)(mergeResults)
          Await.result(res, concurrent.duration.Duration.Inf)
        }
      val timedRes = r.copy(time = System.currentTimeMillis-start)
      params.testCallback.onTestResult("", timedRes)
      timedRes
    } finally {
      stop = true
      tp.shutdown()
    }
  }

  /** Check a set of properties. */
  def checkProperties(prms: Parameters, ps: Properties): Seq[(String,Result)] =
    ps.properties.map { case (name,p) =>
      val testCallback = new TestCallback {
        override def onPropEval(n: String, t: Int, s: Int, d: Int) =
          prms.testCallback.onPropEval(name,t,s,d)
        override def onTestResult(n: String, r: Result) =
          prms.testCallback.onTestResult(name,r)
      }
      val res = check(prms.withTestCallback(testCallback), p)
      (name,res)
    }
}
