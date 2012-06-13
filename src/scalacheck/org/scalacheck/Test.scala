/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2011 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

object Test {

  import util.FreqMap
  import scala.collection.immutable
  import Prop.FM
  import util.CmdLineParser

  /** Test parameters */
  case class Params(
    minSuccessfulTests: Int = 100,

    /** @deprecated Use maxDiscardRatio instead. */
    @deprecated("Use maxDiscardRatio instead.", "1.10")
    maxDiscardedTests: Int = -1,

    minSize: Int = 0,
    maxSize: Int = Gen.Params().size,
    rng: java.util.Random = Gen.Params().rng,
    workers: Int = 1,
    testCallback: TestCallback = new TestCallback {},
    maxDiscardRatio: Float = 5,
    customClassLoader: Option[ClassLoader] = None
  )

  /** Test statistics */
  case class Result(status: Status, succeeded: Int, discarded: Int, freqMap: FM, time: Long = 0) {
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
  sealed case class Proved(args: Prop.Args) extends Status

  /** The property was proved wrong with the given concrete arguments.  */
  sealed case class Failed(args: Prop.Args, labels: Set[String]) extends Status

  /** The property test was exhausted, it wasn't possible to generate enough
   *  concrete arguments satisfying the preconditions to get enough passing
   *  property evaluations. */
  case object Exhausted extends Status

  /** An exception was raised when trying to evaluate the property with the
   *  given concrete arguments. */
  sealed case class PropException(args: Prop.Args, e: Throwable,
    labels: Set[String]) extends Status

  /** An exception was raised when trying to generate concrete arguments
   *  for evaluating the property. */
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

  private def assertParams(prms: Params) = {
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
    try { Left(x) } catch { case e => Right(e) }

  private[scalacheck] lazy val cmdLineParser = new CmdLineParser {
    object OptMinSuccess extends IntOpt {
      val default = Test.Params().minSuccessfulTests
      val names = Set("minSuccessfulTests", "s")
      val help = "Number of tests that must succeed in order to pass a property"
    }
    object OptMaxDiscardRatio extends FloatOpt {
      val default = Test.Params().maxDiscardRatio
      val names = Set("maxDiscardRatio", "r")
      val help =
        "The maximum ratio between discarded and succeeded tests " +
        "allowed before ScalaCheck stops testing a property. At " +
        "least minSuccessfulTests will always be tested, though."
    }
    object OptMinSize extends IntOpt {
      val default = Test.Params().minSize
      val names = Set("minSize", "n")
      val help = "Minimum data generation size"
    }
    object OptMaxSize extends IntOpt {
      val default = Test.Params().maxSize
      val names = Set("maxSize", "x")
      val help = "Maximum data generation size"
    }
    object OptWorkers extends IntOpt {
      val default = Test.Params().workers
      val names = Set("workers", "w")
      val help = "Number of threads to execute in parallel for testing"
    }
    object OptVerbosity extends IntOpt {
      val default = 1
      val names = Set("verbosity", "v")
      val help = "Verbosity level"
    }

    val opts = Set[Opt[_]](
      OptMinSuccess, OptMaxDiscardRatio, OptMinSize,
      OptMaxSize, OptWorkers, OptVerbosity
    )

    def parseParams(args: Array[String]) = parseArgs(args) {
      optMap => Test.Params(
        minSuccessfulTests = optMap(OptMinSuccess),
        maxDiscardRatio = optMap(OptMaxDiscardRatio),
        minSize = optMap(OptMinSize),
        maxSize = optMap(OptMaxSize),
        rng = Test.Params().rng,
        workers = optMap(OptWorkers),
        testCallback = ConsoleReporter(optMap(OptVerbosity))
      )
    }
  }

  /** Tests a property with the given testing parameters, and returns
   *  the test results. */
  def check(params: Params, p: Prop): Result = {

    // maxDiscardedTests is deprecated, but if someone
    // uses it let it override maxDiscardRatio
    val mdr =
      if(params.maxDiscardedTests < 0) params.maxDiscardRatio
      else (params.maxDiscardedTests: Float)/(params.minSuccessfulTests: Float)
    val prms = params.copy( maxDiscardRatio = mdr)

    import prms._
    import scala.actors.Futures.future

    assertParams(prms)
    if(workers > 1)
      assert(!p.isInstanceOf[Commands], "Commands cannot be checked multi-threaded")

    val iterations = math.ceil(minSuccessfulTests / (workers: Double))
    val sizeStep = (maxSize-minSize) / (iterations*workers)
    var stop = false

    def worker(workerIdx: Int) = future {
      params.customClassLoader.map(Thread.currentThread.setContextClassLoader(_))
      var n = 0  // passed tests
      var d = 0  // discarded tests
      var res: Result = null
      var fm = FreqMap.empty[immutable.Set[Any]]
      while(!stop && res == null && n < iterations) {
        val size = (minSize: Double) + (sizeStep * (workerIdx + (workers*(n+d))))
        val propPrms = Prop.Params(Gen.Params(size.round.toInt, prms.rng), fm)
        secure(p(propPrms)) match {
          case Right(e) => res =
            Result(GenException(e), n, d, FreqMap.empty[immutable.Set[Any]])
          case Left(propRes) =>
            fm =
              if(propRes.collected.isEmpty) fm
              else fm + propRes.collected
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
      }
      if (res == null) {
        if (maxDiscardRatio*n > d) Result(Passed, n, d, fm)
        else Result(Exhausted, n, d, fm)
      } else res
    }

    def mergeResults(r1: () => Result, r2: () => Result) = {
      val Result(st1, s1, d1, fm1, _) = r1()
      val Result(st2, s2, d2, fm2, _) = r2()
      if (st1 != Passed && st1 != Exhausted)
        () => Result(st1, s1+s2, d1+d2, fm1++fm2, 0)
      else if (st2 != Passed && st2 != Exhausted)
        () => Result(st2, s1+s2, d1+d2, fm1++fm2, 0)
      else {
        if (s1+s2 >= minSuccessfulTests && maxDiscardRatio*(s1+s2) >= (d1+d2))
          () => Result(Passed, s1+s2, d1+d2, fm1++fm2, 0)
        else
          () => Result(Exhausted, s1+s2, d1+d2, fm1++fm2, 0)
      }
    }

    val start = System.currentTimeMillis
    val results = for(i <- 0 until workers) yield worker(i)
    val r = results.reduceLeft(mergeResults)()
    stop = true
    results foreach (_.apply())
    val timedRes = r.copy(time = System.currentTimeMillis-start)
    prms.testCallback.onTestResult("", timedRes)
    timedRes
  }

  def checkProperties(prms: Params, ps: Properties): Seq[(String,Result)] =
    ps.properties.map { case (name,p) =>
      val testCallback = new TestCallback {
        override def onPropEval(n: String, t: Int, s: Int, d: Int) =
          prms.testCallback.onPropEval(name,t,s,d)
        override def onTestResult(n: String, r: Result) =
          prms.testCallback.onTestResult(name,r)
      }
      val res = check(prms copy (testCallback = testCallback), p)
      (name,res)
    }

}
