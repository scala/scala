package strawman.collection

import javax.imageio.ImageIO

import org.jfree.chart.JFreeChart
import org.jfree.chart.axis.{LogAxis, NumberAxis}
import org.jfree.chart.plot.XYPlot
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.data.xy.{XYSeries, XYSeriesCollection}
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
  def apply(jmhReport: File, targetDir: File): Unit = {
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
              val xySeries = new XYSeries(collectionType)
              // each benchmark has been run with several collection sizes (8, 64, 512, etc.)
              // we add a point for each of these iterations
              for (iteration <- iterations) {
                xySeries.add(
                  (iteration \ "params" \ "size").as[String].toDouble,
                  (iteration \ "primaryMetric" \ "score").as[Double]
                )
              }
              xySeries
            }

        val xAxis = new LogAxis("Size")
        xAxis.setBase(2)
        xAxis.setStandardTickUnits(NumberAxis.createIntegerTickUnits())
        val yAxis = new LogAxis("Execution time (lower is better)")

        val col = new XYSeriesCollection()
        for (series <- seriess) {
          col.addSeries(series)
        }

        val plot = new XYPlot(
          col,
          xAxis, yAxis,
          new XYLineAndShapeRenderer(true, true)
        )

        val chart = new JFreeChart(
          benchmark,
          JFreeChart.DEFAULT_TITLE_FONT,
          plot,
          true
        )

        ImageIO.write(
          chart.createBufferedImage(640, 480),
          "png",
          targetDir / s"$benchmark.png"
        )

      }
  }

}