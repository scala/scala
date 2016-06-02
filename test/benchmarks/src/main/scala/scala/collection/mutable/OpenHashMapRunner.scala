package scala.collection.mutable

import java.io.File
import java.io.PrintWriter

import scala.language.existentials

import org.openjdk.jmh.results.Result
import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.CommandLineOptions
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.runner.options.VerboseMode

import benchmark.JmhRunner

/** Replacement JMH application that runs the [[OpenHashMap]] benchmark.
  * 
  * Outputs the results in a form consumable by a Gnuplot script.
  */
object OpenHashMapRunner extends JmhRunner {
  /** File that will be created for the output data set. */
  private[this] val outputFile = new File(outputDirectory, "OpenHashMap.dat")

  /** Qualifier to add to the name of a memory usage data set. */
  private[this] val memoryDatasetQualifier = "-memory"

  /** Adapter to the JMH result class that simplifies our method calls. */
  private[this] implicit class MyRunResult(r: RunResult) {
    /** Return the dataset label. */
    def label = r.getPrimaryResult.getLabel

    /** Return the value of the JMH parameter for the number of map entries per invocation. */
    def size: String = r.getParams.getParam("size")

    /** Return the operation counts. Not every test tracks this. */
    def operations = Option(r.getSecondaryResults.get("operations"))

    /** Return the number of map entries. */
    def entries = r.getSecondaryResults.get("mapEntries")

    /** Return the memory usage. Only defined if memory usage was measured. */
    def memory = Option(r.getSecondaryResults.get("memory"))
  }

  /** Return the statistics of the given result as a string. */
  private[this] def stats(r: Result[_]) = r.getScore + " " + r.getStatistics.getStandardDeviation


  def main(args: Array[String]) {
    import scala.collection.JavaConversions._
  
    val opts = new CommandLineOptions(args: _*)
    var builder = new OptionsBuilder().parent(opts).jvmArgsPrepend("-Xmx6000m")
    if (!opts.verbosity.hasValue)  builder = builder.verbosity(VerboseMode.SILENT)

    val results = new Runner(builder.build).run()

    /* Sort the JMH results into "data sets", each representing a complete test of one feature.
     * Some results only measure CPU performance; while others also measure memory usage, and
     * thus are split into two data sets.  A data set is distinguished by its label, which is
     * the label of the JMH result, for CPU performance, or that with an added suffix, for memory
     * usage.
     */

    /** Map from data set name to data set. */
    val datasetByName = Map.empty[String, Set[RunResult]]

    /** Ordering for the results within a data set. Orders by increasing number of map entries. */
    val ordering = Ordering.by[RunResult, Int](_.size.toInt)

    def addToDataset(key: String, result: RunResult): Unit =
      datasetByName.getOrElseUpdate(key, SortedSet.empty(ordering)) += result

    results.foreach { result =>
      addToDataset(result.label, result)

      // Create another data set for trials that track memory usage
      if (result.memory.isDefined)
        addToDataset(result.label + memoryDatasetQualifier, result)
    }

    //TODO Write out test parameters
    //    val jvm = params.getJvm
    //    val jvmArgs = params.getJvmArgs.mkString(" ")

    val f = new PrintWriter(outputFile, "UTF-8")
    try {
      datasetByName.foreach(_ match {
        case (label: String, dataset: Iterable[RunResult]) =>
          outputDataset(f, label, dataset)
        })
    } finally {
      f.close()
    }
  }
  
  private[this] def outputDataset(f: PrintWriter, label: String, dataset: Iterable[RunResult]) {
    f.println(s"# [$label]")

    val isMemoryUsageDataset = label.endsWith(memoryDatasetQualifier)
    dataset.foreach { r =>
      f.println(r.size + " " + (
        if (isMemoryUsageDataset && !r.memory.get.getScore.isInfinite)
          stats(r.entries) + " " + stats(r.memory.get)
        else
          stats(r.operations getOrElse r.getPrimaryResult)
      ))
    }

    f.println(); f.println()  // data set separator
  }
}
