/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003-2006, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$


package scala.testing


import compat.Platform

/** <code>Benchmark</code> can be used to quickly turn an existing
 *  class into a benchmark. Here is a short example:
 *
 *  <pre>
 *  <b>object</b> sort1 <b>extends</b> Sorter <b>with</b> Benchmark {
 *    <b>def</b> run = sort(List.range(1, 1000))
 *  }
 *  </pre>
 *  <p>
 *    The run method has to be defined by the user, who will perform
 *    the timed operation there.
 *    Run the benchmark as follows:
 *  </p>
 *  <pre>
 *  &gt; scala sort1 5 times.log
 *  </pre>
 *  <p>
 *    This will run the benchmark 5 times and log the execution times in
 *    a file called <code>times.log</code>
 *  </p>
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
    for (val i <- List.range(1, noTimes + 1)) yield {
      val startTime = Platform.currentTime
      var i = 0; while(i < multiplier) {
        run()
        i = i + 1
      }
      val stopTime = Platform.currentTime
      Platform.collectGarbage

      stopTime - startTime
    }

  /**
   * The entry point. It takes two arguments: the number of
   * consecutive runs, and the name of a log file where to
   * append the times.
   */
  def main(args: Array[String]): Unit = {
    if (args.length > 1) {
      val logFile = new java.io.FileWriter(args(1), true) // append, not overwrite
      if(args.length >= 3)
         multiplier = args(2).toInt
      logFile.write(getClass().getName())
      for (val t <- runBenchmark(args(0).toInt))
        logFile.write("\t\t" + t)

      logFile.write(Platform.EOL)
      logFile.flush()
    } else {
      Console.println("Usage: scala benchmarks.program <runs> <logfile>")
      Console.println("   or: scala benchmarks.program <runs> <logfile> <multiplier>")
    }
  }
}

