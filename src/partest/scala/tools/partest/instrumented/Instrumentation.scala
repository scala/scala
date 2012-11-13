/* NEST (New Scala Test)
 * Copyright 2007-2013 LAMP/EPFL
 * @author Grzegorz Kossakowski
 */

package scala.tools.partest.instrumented

import scala.collection.JavaConverters._

case class MethodCallTrace(className: String, methodName: String, methodDescriptor: String) {
  override def toString(): String = className + "." + methodName + methodDescriptor
}
object MethodCallTrace {
  implicit val ordering: Ordering[MethodCallTrace] = Ordering.by(x => (x.className, x.methodName, x.methodDescriptor))
}

/**
 * An object that controls profiling of instrumented byte-code. The instrumentation is achieved
 * by using `java.lang.instrument` package. The instrumentation agent can be found in
 * `scala.tools.partest.javaagent` package.
 *
 * At the moment the following classes are being instrumented:
 *   * all classes with empty package
 *   * all classes from scala package (except for classes responsible for instrumentation)
 *
 * The canonical way of using instrumentation is have a test-case in `files/instrumented` directory.
 * The following code in main:
 *
 * {{{
 * import scala.tools.partest.instrumented.Instrumentation._
 * def main(args: Array[String]): Unit = {
 *   startProfiling()
 *   // should box the boolean
    println(true)
    stopProfiling()
    printStatistics()
 * }
 * }}}
 *
 *
 * should print:
 *
 * {{{
 * true
 * Method call statistics:
 * scala/Predef$.println(Ljava/lang/Object;)V: 1
 * scala/runtime/BoxesRunTime.boxToBoolean(Z)Ljava/lang/Boolean;: 1
 * }}}
 */
object Instrumentation {

  type Statistics = Map[MethodCallTrace, Int]

  def startProfiling(): Unit = Profiler.startProfiling()
  def stopProfiling(): Unit = Profiler.stopProfiling()
  def resetProfiling(): Unit = Profiler.resetProfiling()
  def isProfiling(): Boolean = Profiler.isProfiling()

  def getStatistics: Statistics = {
    val isProfiling = Profiler.isProfiling()
    if (isProfiling) {
      Profiler.stopProfiling()
    }
    val stats = Profiler.getStatistics().asScala.toSeq.map {
      case (trace, count) => MethodCallTrace(trace.className, trace.methodName, trace.methodDescriptor) -> count.intValue
    }
    val res = Map(stats: _*)
    if (isProfiling) {
      Profiler.startProfiling()
    }
    res
  }

  val standardFilter: MethodCallTrace => Boolean = t => {
    // ignore all calls to Console trigger by printing
    t.className != "scala/Console$" &&
    // console accesses DynamicVariable, let's discard it too
    !t.className.startsWith("scala/util/DynamicVariable")
  }

  // Used in tests.
  def printStatistics(stats: Statistics = getStatistics, filter: MethodCallTrace => Boolean = standardFilter): Unit = {
    val stats = getStatistics
    println("Method call statistics:")
    val toBePrinted = stats.toSeq.filter(p => filter(p._1)).sortBy(_._1)
    // <count> <trace>
    val format = "%5d  %s\n"
    toBePrinted foreach {
      case (trace, count) => printf(format, count, trace)
    }
  }

}
