/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2009, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.testing


import compat.Platform

/** <p>
 *    <code>Benchmark</code> can be used to quickly turn an existing
 *    class into a benchmark. Here is a short example:
 *  </p><pre>
 *  <b>object</b> sort1 <b>extends</b> Sorter <b>with</b> Benchmark {
 *    <b>def</b> run = sort(List.range(1, 1000))
 *  }
 *  </pre>
 *  <p>
 *    The <code>run</code> method has to be defined by the user, who
 *    will perform the timed operation there.
 *    Run the benchmark as follows:
 *  </p>
 *  <pre>
 *  &gt; scala sort1 5 times.log
 *  </pre>
 *  <p>
 *    This will run the benchmark 5 times and log the execution times in
 *    a file called <code>times.log</code>
 *  </p>
 *
 *  @author Iulian Dragos, Burak Emir
 */
trait Benchmark {

  /** this method should be implemented by the concrete benchmark */
  def run()

  var multiplier = 1

  /** Run the benchmark the specified number of times
   *  and return a list with the execution times in milliseconds
   *  in reverse order of the execution
   *
   *  @param noTimes ...
   *  @return        ...
   */
  def runBenchmark(noTimes: Int): List[Long] =
    for (i <- List.range(1, noTimes + 1)) yield {
      val startTime = Platform.currentTime
      var i = 0; while (i < multiplier) {
        run()
        i += 1
      }
      val stopTime = Platform.currentTime
      Platform.collectGarbage

      stopTime - startTime
    }

  /** a string that is written at the beginning of the output line
   *   that contains the timings. By default, this is the class name.
   */
  def prefix: String = getClass().getName()

  /**
   * The entry point. It takes two arguments (n),
   *  and an optional argument multiplier (mult).
   *  (n) is the number of consecutive runs,
   *  if (mult) is present, the n runs are repeated (mult)
   *  times.
   */
  def main(args: Array[String]) {
    if (args.length > 0) {
      val logFile = new java.io.OutputStreamWriter(System.out)
      if (args.length > 1) multiplier = args(1).toInt
      logFile.write(prefix)
      for (t <- runBenchmark(args(0).toInt))
        logFile.write("\t\t" + t)

      logFile.write(Platform.EOL)
      logFile.flush()
    } else {
      println("Usage: scala benchmarks.program <runs> ")
      println("   or: scala benchmarks.program <runs> <multiplier>")
      println("""
    The benchmark is run <runs> times, forcing a garbage collection between runs. The optional
    <multiplier> causes the benchmark to be repeated <multiplier> times, each time for <runs>
    executions.
      """)
    }
  }
}

