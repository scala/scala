/*-------------------------------------------------------------------------*\
**  ScalaCheck                                                             **
**  Copyright (c) 2007-2018 Rickard Nilsson. All rights reserved.          **
**  http://www.scalacheck.org                                              **
**                                                                         **
**  This software is released under the terms of the Revised BSD License.  **
**  There is NO WARRANTY. See the file LICENSE for the full text.          **
\*------------------------------------------------------------------------ */

package org.scalacheck

import Prop.Arg

object Test {

  import util.{FreqMap, CmdLineParser, ConsoleReporter}

  /** Test parameters used by the check methods. Default
   *  parameters are defined by [[Test.Parameters.default]]. */
  sealed abstract class Parameters {
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
     *  ScalaCheck gives up and discards the whole property (with status
     *  [[Test.Exhausted]]). Additionally, ScalaCheck will always allow
     *  at least `minSuccessfulTests * maxDiscardRatio` discarded tests, so the
     *  resulting discard ratio might be higher than `maxDiscardRatio`. */
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

    /** An optional regular expression to filter properties on. */
    val propFilter: Option[String]

    /** Create a copy of this [[Test.Parameters]] instance with
     *  [[Test.Parameters.propFilter]] set to the specified regular expression
     *  filter. */
    def withPropFilter(propFilter: Option[String]): Parameters = cp(
      propFilter = propFilter
    )

    /** Initial seed to use for testing. */
    val initialSeed: Option[rng.Seed]

    def withInitialSeed(seed: rng.Seed): Parameters =
      cp(initialSeed = Some(seed))

    def withInitialSeed(n: Long): Parameters =
      cp(initialSeed = Some(rng.Seed(n)))

    def withNoInitialSeed: Parameters =
      cp(initialSeed = None)

    // private since we can't guarantee binary compatibility for this one
    private case class cp(
      minSuccessfulTests: Int = minSuccessfulTests,
      minSize: Int = minSize,
      maxSize: Int = maxSize,
      workers: Int = workers,
      testCallback: TestCallback = testCallback,
      maxDiscardRatio: Float = maxDiscardRatio,
      customClassLoader: Option[ClassLoader] = customClassLoader,
      propFilter: Option[String] = propFilter,
      initialSeed: Option[rng.Seed] = initialSeed
    ) extends Parameters

    override def toString = s"Parameters${cp.toString.substring(2)}"
  }

  /** Test parameters used by the check methods. Default
   *  parameters are defined by [[Test.Parameters.default]]. */
  object Parameters {
    /** Default test parameters. Can be overriden if you need to
     *  tweak the parameters:
     *
     *  {{{
     *  val myParams = Parameters.default
     *    .withMinSuccessfulTests(600)
     *    .withMaxDiscardRatio(8)
     *  }}} */
    val default: Parameters = new Parameters {
      val minSuccessfulTests: Int = 100
      val minSize: Int = 0
      val maxSize: Int = Gen.Parameters.default.size
      val workers: Int = 1
      val testCallback: TestCallback = new TestCallback {}
      val maxDiscardRatio: Float = 5
      val customClassLoader: Option[ClassLoader] = None
      val propFilter = None
      val initialSeed: Option[rng.Seed] = None
    }

    /** Verbose console reporter test parameters instance. */
    val defaultVerbose: Parameters = default.withTestCallback(ConsoleReporter(2))
  }

  /** Test statistics */
  final case class Result(
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

  trait TestCallback { self =>
    /** Called each time a property is evaluated */
    def onPropEval(name: String, threadIdx: Int, succeeded: Int,
      discarded: Int): Unit = ()

    /** Called whenever a property has finished testing */
    def onTestResult(name: String, result: Result): Unit = ()

    def chain(testCallback: TestCallback): TestCallback = new TestCallback {
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

  private[scalacheck] lazy val cmdLineParser = new CmdLineParser {
    object OptMinSuccess extends IntOpt {
      val default = Parameters.default.minSuccessfulTests
      val names = Set("minSuccessfulTests", "s")
      val help = "Number of tests that must succeed in order to pass a property"
    }
    object OptMaxDiscardRatio extends FloatOpt {
      val default = Parameters.default.maxDiscardRatio
      val names = Set("maxDiscardRatio", "r")
      val help =
        "The maximum ratio between discarded and succeeded tests " +
        "allowed before ScalaCheck stops testing a property. At " +
        "least minSuccessfulTests will always be tested, though."
    }
    object OptMinSize extends IntOpt {
      val default = Parameters.default.minSize
      val names = Set("minSize", "n")
      val help = "Minimum data generation size"
    }
    object OptMaxSize extends IntOpt {
      val default = Parameters.default.maxSize
      val names = Set("maxSize", "x")
      val help = "Maximum data generation size"
    }
    object OptWorkers extends IntOpt {
      val default = Parameters.default.workers
      val names = Set("workers", "w")
      val help = "Number of threads to execute in parallel for testing"
    }
    object OptVerbosity extends IntOpt {
      val default = 1
      val names = Set("verbosity", "v")
      val help = "Verbosity level"
    }

    object OptPropFilter extends OpStrOpt {
      val default = Parameters.default.propFilter
      val names = Set("propFilter", "f")
      val help = "Regular expression to filter properties on"
    }

    val opts = Set[Opt[_]](
      OptMinSuccess, OptMaxDiscardRatio, OptMinSize,
      OptMaxSize, OptWorkers, OptVerbosity,
      OptPropFilter
    )

    def parseParams(args: Array[String]): (Parameters => Parameters, List[String]) = {
      val (optMap, us) = parseArgs(args)
      val params = (p: Parameters) => p
        .withMinSuccessfulTests(optMap(OptMinSuccess): Int)
        .withMaxDiscardRatio(optMap(OptMaxDiscardRatio): Float)
        .withMinSize(optMap(OptMinSize): Int)
        .withMaxSize(optMap(OptMaxSize): Int)
        .withWorkers(optMap(OptWorkers): Int)
        .withPropFilter(optMap(OptPropFilter): Option[String])
        .withTestCallback(ConsoleReporter(optMap(OptVerbosity)): TestCallback)
      (params, us)
    }
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
    assertParams(params)

    val iterations = math.ceil(minSuccessfulTests / workers.toDouble)
    val sizeStep = (maxSize-minSize) / (iterations*workers)
    var stop = false
    //val seed = p.fixedSeed.getOrElse(rng.Seed.random)
    //val genPrms = Gen.Parameters.default.withInitialSeed(seed)
    val genPrms = Gen.Parameters.default

    def workerFun(workerIdx: Int): Result = {
      var n = 0  // passed tests
      var d = 0  // discarded tests
      var res: Result = null
      var fm = FreqMap.empty[Set[Any]]

      def isExhausted = d > minSuccessfulTests * maxDiscardRatio

      while(!stop && res == null && n < iterations) {
        val size = minSize.toDouble + (sizeStep * (workerIdx + (workers*(n+d))))
        val propRes = p(genPrms.withSize(size.round.toInt))
        fm = if(propRes.collected.isEmpty) fm else fm + propRes.collected
        propRes.status match {
          case Prop.Undecided =>
            d += 1
            testCallback.onPropEval("", workerIdx, n, d)
            if (isExhausted) res = Result(Exhausted, n, d, fm)
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
        if (isExhausted) Result(Exhausted, n, d, fm)
        else Result(Passed, n, d, fm)
      } else res
    }

    val start = System.currentTimeMillis
    val r = Platform.runWorkers(params, workerFun, () => stop = true)
    val timedRes = r.copy(time = System.currentTimeMillis-start)
    params.testCallback.onTestResult("", timedRes)
    timedRes
  }

  import scala.util.matching.Regex
  /** Returns the result of filtering a property name by a supplied regular expression.
    *
    *  @param propertyName The name of the property to be filtered.
    *  @param regex The regular expression to filter the property name by.
    *  @return true if the regular expression matches the property name, false if not.
    */
  def matchPropFilter(propertyName: String, regex: Regex): Boolean = {
    regex.findFirstIn(propertyName).isDefined
  }

  /** Check a set of properties. */
  def checkProperties(prms: Parameters, ps: Properties): collection.Seq[(String,Result)] = {
    val params = ps.overrideParameters(prms)
    val propertyFilter = prms.propFilter.map(_.r)

    ps.properties.filter {
      case (name, _) => propertyFilter.fold(true)(matchPropFilter(name, _))
    } map {
      case (name, p)  =>
        val testCallback = new TestCallback {
          override def onPropEval(n: String, t: Int, s: Int, d: Int) =
            params.testCallback.onPropEval(name,t,s,d)
          override def onTestResult(n: String, r: Result) =
            params.testCallback.onTestResult(name,r)
        }

        val res = check(params.withTestCallback(testCallback), p)
        (name,res)
    }
  }
}
