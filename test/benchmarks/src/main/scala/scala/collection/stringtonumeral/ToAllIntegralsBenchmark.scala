package scala.collection

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit
import scala.util.Try

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 15)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ToAllIntegralsBenchmark {

  @Param(Array("", "1", "-1", "+010", "-0001000", "+000012345678912", "-000000000200000000003123456"))
  var parsee: String = _

  @Benchmark def parseInt(bh: Blackhole): Unit = {
    bh.consume(parsee.parseInt)
  }

  @Benchmark def toInt(bh: Blackhole): Unit = {
    bh.consume(parsee.toInt)
  }

  @Benchmark def checkedToInt(bh: Blackhole): Unit = {
    try {
      val b = parsee.toInt
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

  @Benchmark def parseByte(bh: Blackhole): Unit = {
    bh.consume(parsee.parseByte)
  }

  @Benchmark def toByte(bh: Blackhole): Unit = {
    bh.consume(parsee.toByte)
  }

  @Benchmark def checkedToByte(bh: Blackhole): Unit = {
    try {
      val b = parsee.toByte
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

  @Benchmark def parseShort(bh: Blackhole): Unit = {
    bh.consume(parsee.parseShort)
  }

  @Benchmark def toShort(bh: Blackhole): Unit = {
    bh.consume(parsee.toShort)
  }

  @Benchmark def checkedToShort(bh: Blackhole): Unit = {
    try {
      val b = parsee.toShort
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

  @Benchmark def parseLong(bh: Blackhole): Unit = {
    bh.consume(parsee.parseLong)
  }

  @Benchmark def toLong(bh: Blackhole): Unit = {
    bh.consume(parsee.toLong)
  }

  @Benchmark def checkedToLong(bh: Blackhole): Unit = {
    try {
      val b = parsee.toLong
      bh.consume(b)
    } catch {
      case e: NumberFormatException => bh.consume(e)
    }
  }

}
