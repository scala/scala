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

/** Replacement JMH application that runs the [[OpenHashMap]] benchmark.
  * 
  * Outputs the results in a form consumable by a Gnuplot script.
  */
object OpenHashMapRunner extends JmhRunner {
  /** File that will be created for the output data set. */
  private[this] val outputFile = new File(outputDirectory, "OpenHashMap.dat")

  /** Qualifier to add to the name of a memory usage data set. */
  private[this] val memoryDatasetQualifer = " memory"

  /** Name of the JMH parameter for the number of map entries per invocation. */
  private[this] val sizeParamName = "size"

  /** Name of the JMH auxiliary counter that collects operation counts. */
  private[this] val operationsAuxCounterName = "operations"

  /** Name of the JMH auxiliary counter that collects memory usage. */
  private[this] val memoryAuxCounterName = "memory"

  /** Name of the JMH auxiliary counter that collects the number of map entries. */
  private[this] val entriesAuxCounterName = "mapEntries"

  def main(args: Array[String]) {
    import scala.collection.JavaConversions._
    import scala.language.existentials
  
    val opts = new CommandLineOptions(args: _*)
    var builder = new OptionsBuilder().parent(opts)
      .jvmArgsPrepend("-Xmx6000m")
    if (!opts.verbosity.hasValue)  builder = builder.verbosity(VerboseMode.SILENT)

    val results = new Runner(builder.build).run()

    // Sort the results

    /** Map from data set name to data set. */
    val datasetByName = Map.empty[String, Set[RunResult]]

    /** Ordering for the results within a data set. Orders by increasing number of map entries. */
    val ordering = Ordering.by[RunResult, Int](_.getParams.getParam(sizeParamName).toInt)

    def addToDataset(result: RunResult, key: String): Unit =
      datasetByName.get(key)
        .getOrElse({ val d = SortedSet.empty(ordering); datasetByName.put(key, d); d }) += result

    results.foreach { result: RunResult ⇒
      addToDataset(result, result.getPrimaryResult.getLabel)

      // Create another data set for trials that track memory usage
      if (result.getSecondaryResults.containsKey(memoryAuxCounterName))
        addToDataset(result, result.getPrimaryResult.getLabel + memoryDatasetQualifer)
    }

    //TODO Write out test parameters
    //    val jvm = params.getJvm
    //    val jvmArgs = params.getJvmArgs.mkString(" ")

    val f = new PrintWriter(outputFile, "UTF-8")
    try {
      datasetByName.foreach(_ match { case (label: String, dataset: Iterable[RunResult]) ⇒ {
        f.format("# [%s]\n", label)

        val isMemoryUsageDataset = label.contains(memoryDatasetQualifer)
        dataset.foreach { result ⇒
          val size = result.getParams.getParam(sizeParamName)
          val secondaryResults = result.getSecondaryResults
          if (isMemoryUsageDataset) {
            val memoryResult = secondaryResults.get(memoryAuxCounterName)
            val entriesResult = secondaryResults.get(entriesAuxCounterName)
            f.format("%s %f %f %f %f\n", size,
              Double.box(entriesResult.getScore), Double.box(entriesResult.getStatistics.getStandardDeviation),
              Double.box(memoryResult.getScore), Double.box(memoryResult.getStatistics.getStandardDeviation))
          }
          else {
            if (secondaryResults.containsKey(operationsAuxCounterName)) {
              val operationsResult = secondaryResults.get(operationsAuxCounterName)
              f.format("%s %f %f\n", size,
                Double.box(operationsResult.getScore), Double.box(operationsResult.getStatistics.getStandardDeviation))
            } else {
              val primary = result.getPrimaryResult
              f.format("%s %f %f\n", size,
                Double.box(primary.getScore), Double.box(primary.getStatistics.getStandardDeviation))
            }
          }
        }

        f.println(); f.println()  // data set separator
      }})
    } finally {
      f.close()
    }
  }
}
