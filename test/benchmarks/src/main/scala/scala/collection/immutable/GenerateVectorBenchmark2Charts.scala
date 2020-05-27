/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import java.nio.file.Files
import java.text.DecimalFormat

import scala.jdk.CollectionConverters._

/**
  * Generate line charts for Vector benchmarks.
  *
  * Run benchmark and collect raw data:
  *   bench/jmh:run -rff vector-bench.csv -jvmArgs "-Xms256M -Xmx1G" scala.collection.immutable.VectorBenchmark2
  *
  * Generate diagram data:
  *   bench/runMain scala.collection.immutable.GenerateVectorBenchmark2Charts test/benchmarks/vector-bench.csv test/benchmarks/vector-bench-data.js
  */
object GenerateVectorBenchmark2Charts extends App {

  case class Result(name: String, score: Double, error: Double, size: Int)

  def load(path: String): Map[String, IndexedSeq[Result]] = {
    val f = new File(path)
    if(f.getName.endsWith("*")) {
      val dir = f.getParentFile
      val prefix = f.getName.init
      val files = dir.listFiles().toSeq.filter(_.getName.startsWith(prefix))
      files.foldLeft(Map.empty[String, IndexedSeq[Result]])({ (m, f) => m ++ load(f) })
    } else load(f)
  }

  def load(file: File): Map[String, IndexedSeq[Result]] = {
    println(s"Loading $file...")
    Files.readAllLines(file.toPath).asScala.drop(1).map { s =>
      val a = s.split(',')
      def unquote(s: String): String = s.substring(1, s.length-1)
      def local(s: String): String = {
        val i = s.lastIndexOf('.')
        if(i < 0) s else s.substring(i+1)
      }
      Result(local(unquote(a(0))), a(4).toDouble, a(5).toDouble, a(7).toInt)
    }.toIndexedSeq.groupBy(_.name)
  }

  val data = args.toSeq.init.foldLeft(Map.empty[String, IndexedSeq[Result]])({ (m, s) => m ++ load(s) })
  val fmt3 = new DecimalFormat("###,###.###")

  def fmtTime(ns: Double, by: Double, withUnit: Boolean): String = {
    val (s, u) =
      if(by >= 1000000000d) (fmt3.format(ns/1000000000d), "s")
      else if(by >= 1000000d) (fmt3.format(ns/1000000d), "ms")
      else if(by >= 1000d) (fmt3.format(ns/1000d), "μs")
      else (fmt3.format(ns), "ns")
    if(withUnit) s"$s $u"
    else s
  }

  def fmtSize(i: Int): String = {
    if(i >= 1000000000) s"${i/1000000000}B"
    else if(i >= 1000000) s"${i/1000000}M"
    else if(i >= 1000) s"${i/1000}K"
    else s"$i"
  }

  def printChartData(out: PrintWriter, name: String, rss: IndexedSeq[IndexedSeq[Result]], seriesNames: IndexedSeq[String]): Unit = {
    println(s"""drawChart(new ChartData("$name", benchmarkData.$name));""")
    val sizes = rss.flatten.map(_.size).toSet.toIndexedSeq.sorted
    val bySize = rss.map(_.iterator.map(r => (r.size, r)).toMap)
    val benchmarkNames = rss.map(_.head.name)

    val minScore = rss.flatten.map(_.score).min
    val maxScore = rss.flatten.map(_.score).max
    var timeFactor =
      if(minScore > 1000000d) 1000000L
      else if(minScore > 1000d) 1000L
      else 1L
    val timeUnit = timeFactor match {
      case 1L => "ns"
      case 1000L => "μs"
      case 1000000L => "ms"
    }

    out.println(s"  $name: {")
    out.println(s"    rows: [")
    var first = true
    sizes.foreach { size =>
      if(!first) out.println(",")
      else first = false
      val sizeStr = fmtSize(size)
      val forSize = bySize.map(_.get(size))
      val minScore = forSize.map(_.map(_.score)).flatten.min
      val line = forSize.zipWithIndex.map { case (ro, i) => ro.map { r =>
        Seq(
          r.score/timeFactor,
          (r.score-r.error)/timeFactor,
          (r.score+r.error)/timeFactor,
          "\"Size: " + sizeStr + "<br/>" + seriesNames(i) + ": <b>" + fmtTime(r.score, minScore, true) + "</b> ± " + fmtTime(r.error, minScore, false) + "\""
        )
      }.getOrElse(Seq(null, null, null, null)) }.flatten
      out.print(s"      [$size, ${line.mkString(", ")}]")
    }
    out.println()
    out.println("    ],")
    out.println("    names: [" + benchmarkNames.map(s => "\""+s+"\"").mkString(", ") + "],")
    out.println("    timeUnit: \""+timeUnit+"\"")
    out.print("  }")
  }

  val baseNames = data.keySet.filter(_.startsWith("nv")).map(_.drop(2)).toSeq.sorted
  val comparisons = baseNames.map { s =>
    data.get("v"+s).map(v => (s, data("nv"+s), v))
  }.flatten

  val out = new PrintWriter(new BufferedWriter(new FileWriter(args(args.length-1))))
  out.println("var benchmarkData = {")

  var first = true
  val seriesNames = IndexedSeq("Old Vector", "New Vector")
  for((baseName, nvRes, vRes) <- comparisons) {
    if(!first) out.println(",")
    else first = false
    printChartData(out, baseName, IndexedSeq(vRes, nvRes), seriesNames)
  }

  out.println()
  out.println("};")
  out.close()
}
