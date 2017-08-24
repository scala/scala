package scala

import benchmark.JmhRunner
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.OptionsBuilder

object RegexUnapplyRunner extends JmhRunner {
  val groupingTestFunctionName = s"groupingBenchmark"

  def main(args: Array[String]): Unit = {
    val groupingTestOpt = new OptionsBuilder()
      .include(groupingTestFunctionName)
      .addProfiler("gc")
      .build()

    new Runner(groupingTestOpt).run()

    val restTestOpt = new OptionsBuilder()
      .exclude(groupingTestFunctionName)
      .addProfiler("gc")
      .param("groupCount", "1")
      .build()

    new Runner(restTestOpt).run()
  }
}
