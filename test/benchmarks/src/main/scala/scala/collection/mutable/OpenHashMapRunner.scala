package scala.collection.mutable

import java.io.BufferedWriter
import java.io.File
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.io.PrintWriter
import scala.collection.JavaConversions
import scala.language.existentials
import org.openjdk.jmh.results.RunResult
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.options.CommandLineOptions
import org.openjdk.jmh.runner.options.Options
import benchmark.JmhRunner
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.runner.options.VerboseMode
import org.openjdk.jmh.results.Result

/** Replacement JMH application that runs the [[OpenHashMap]] benchmark.
  * 
  * Outputs the results in a form consumable by a Gnuplot script.
  */
object OpenHashMapRunner extends JmhRunner {
  /** File that will be created for the output data set. */
  private[this] val outputFile = new File(outputDirectory, "OpenHashMap.dat")

  /** Qualifier to add to the name of a memory usage data set. */
  private[this] val memoryDatasetQualifier = "-memory"

  private[this] implicit class MyRunResult(r: RunResult) {
    /** Return the dataset label. */
    def label = r.getPrimaryResult.getLabel

    /** Return the value of the JMH parameter for the number of map entries per invocation. */
    def size: String = r.getParams.getParam("size")

    /** Return the operation counts. */
    def operations = Option(r.getSecondaryResults.get("operations"))

    /** Return the number of map entries. */
    def entries = r.getSecondaryResults.get("mapEntries")

    /** Return the memory usage. */
    def memory = Option(r.getSecondaryResults.get("memory"))
  }

  /** Return the statistics of the given result as a string. */
  private[this] def stats(r: Result[_]) = r.getScore + " " + r.getStatistics.getStandardDeviation


  def main(args: Array[String]) {
    import scala.collection.JavaConversions._
    import scala.language.existentials
  
    val opts = new CommandLineOptions(args: _*)
    var builder = new OptionsBuilder().parent(opts).jvmArgsPrepend("-Xmx6000m")
    if (!opts.verbosity.hasValue)  builder = builder.verbosity(VerboseMode.SILENT)

    val results = new Runner(builder.build).run()

    // Sort the results

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
      datasetByName.foreach(_ match { case (label: String, dataset: Iterable[RunResult]) => {
        f.println(s"# [$label]")

        val isMemoryUsageDataset = label.endsWith(memoryDatasetQualifier)
        dataset.foreach { r =>
          f.println(r.size + " " + (
            if (isMemoryUsageDataset)
              stats(r.entries) + " " + stats(r.memory.get)
            else
              stats(r.operations getOrElse r.getPrimaryResult)
          ))
        }

        f.println(); f.println()  // data set separator
      }})
    } finally {
      f.close()
    }
  }
}
