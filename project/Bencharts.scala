package strawman.collection

import javax.imageio.ImageIO

import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.{LogAxis, LogarithmicAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYErrorRenderer
import org.jfree.data.xy.{YIntervalSeries, YIntervalSeriesCollection}
import play.api.libs.json.{JsObject, Json}
import sbt._

object Bencharts {

  /**
    * Generate charts from the result of a JMH execution.
    *
    * Benchmarks that have the same name (e.g. `cons`) are grouped
    * into a single chart with one series for each.
    *
    * @param jmhReport JMH results report
    * @param targetDir Directory in which the images will be written
    */
  def apply(jmhReport: File, yAxisTitle: String, targetDir: File): Unit = {
    val json = Json.parse(IO.read(jmhReport))

    json.as[List[JsObject]]
      .groupBy { result =>
        val name = (result \ "benchmark").as[String]
        val benchmark = name.reverse.takeWhile(_ != '.').reverse
        benchmark // Benchmark name (e.g. "cons", "foreach", "map")
      }
      .foreach { case (benchmark, results) =>
        val seriess =
          results
            // group by concrete collection type
            .groupBy(result => (result \ "benchmark").as[String].stripSuffix(benchmark))
            .map { case (collectionType, iterations) =>
              val ySeries = new YIntervalSeries(collectionType)
              // each benchmark has been run with several collection sizes (8, 64, 512, etc.)
              // we add a point for each of these iterations
              for (iteration <- iterations) {
                ySeries.add(
                  (iteration \ "params" \ "size").as[String].toDouble,
                  (iteration \ "primaryMetric" \ "score").as[Double],
                  (iteration \ "primaryMetric" \ "scoreConfidence").apply(0).as[Double],
                  (iteration \ "primaryMetric" \ "scoreConfidence").apply(1).as[Double]
                )
              }
              ySeries
            }

        val xAxis = new LogarithmicAxis("Size")
        xAxis.setAllowNegativesFlag(true)
        val yAxis = new LogarithmicAxis(yAxisTitle)
        yAxis.setAllowNegativesFlag(true)

        val col = new YIntervalSeriesCollection()
        val renderer = new XYErrorRenderer
        for ((series, i) <- seriess.zipWithIndex) {
          col.addSeries(series)
          renderer.setSeriesLinesVisible(i, true)
        }

        val plot = new XYPlot(
          col,
          xAxis, yAxis,
          renderer
        )

        val chart = new JFreeChart(
          benchmark,
          JFreeChart.DEFAULT_TITLE_FONT,
          plot,
          true
        )

        ImageIO.write(
          chart.createBufferedImage(800, 600),
          "png",
          targetDir / s"$benchmark.png"
        )

      }
  }

}