package scala.collection.immutable

import java.io.{File, PrintWriter}

import benchmark.JmhRunner
import org.openjdk.jmh.results.{Result, RunResult}
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.{CommandLineOptions, OptionsBuilder, VerboseMode}

import scala.language.existentials

/** Replacement JMH application that runs the [[Set]] benchmark.
  *
  * Outputs the results in a form consumable by a Gnuplot script.
  */
object SetRunner extends JmhRunner {
  /** File that will be created for the output data set. */
  private[this] val outputFile = new File(outputDirectory, "SetDiffComaparsion.dat")

  /** Adapter to the JMH result class that simplifies our method calls. */
  private[this] implicit class MyRunResult(r: RunResult) {
    /** Return the dataset label. */
    def label = r.getPrimaryResult.getLabel

    /** Return the number of values. */
    def size = r.getParams.getParam("size")

    /** Return the ratio counts. */
    def ratio = r.getParams.getParam("ratio")

    /** Return the intersection. */
    def intersection = r.getParams.getParam("intersection")

  }

  /** Return the statistics of the given result as a string. */
  private[this] def stats(r: Result[_]) = r.getScore + " " + r.getStatistics.getStandardDeviation

  def main(args: Array[String]) {

    val opts = new CommandLineOptions(args: _*)
    var builder = new OptionsBuilder().parent(opts).jvmArgsPrepend("-Xmx6000m")
    if (!opts.verbosity.hasValue) builder = builder.verbosity(VerboseMode.SILENT)

    val results = new Runner(builder.build).run()

    val f = new PrintWriter(outputFile, "UTF-8")
    try {
      results.forEach(result => outputDataset(f, result.label, result))
    } finally {
      f.close()
    }
  }

  private[this] def outputDataset(f: PrintWriter, label: String, r: RunResult) {
    f.println(s"# [$label]")
    f.println(r.size + "\t" + r.ratio + "\t" + r.intersection + "\t" + r.getPrimaryResult.getScore)
    f.println("\n")
  }
}
