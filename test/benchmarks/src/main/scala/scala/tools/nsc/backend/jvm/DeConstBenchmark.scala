package scala.tools.nsc.backend.jvm

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Level, Measurement, Mode, OperationsPerInvocation, OutputTimeUnit, Param, Scope, Setup, State, Threads, Warmup}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random


@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DeConstFloatBenchmark {

  @Param(Array("0", "1", "2", "3", "-1", "NaN", "Random"))
  var num: String = _
  var floats: Array[Float] = _
  var doubles: Array[Double] = _


  @Setup(Level.Trial) def setup(): Unit = {
    val size = 1000
    val (ffn, dfn) = num match {
      case "Random" =>
        val r = new Random()
        ((_: Int) => r.nextFloat(), (_: Int) => r.nextDouble())
      case "NaN" =>
        ((_: Int) => Float.NaN, (_: Int) => Double.NaN)
      case x =>
        val v = x.toDouble
        ((_: Int) => v.toFloat, (_: Int) => v)
    }
    floats = Array.tabulate(1000)(ffn)
    doubles = Array.tabulate(1000)(dfn)
  }

  def FCONST_0 = 0

  def FCONST_1 = 1

  def FCONST_2 = 2

  def DCONST_0 = 3

  def DCONST_1 = 4

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstOrig(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstOrig(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BdconstOrig(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      dconstOrig(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstRaw(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstRaw(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BdconstRaw(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      dconstRaw(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstTyped(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstTyped(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BdconstTyped(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      dconstTyped(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstTyped2(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstTyped2(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BdconstTyped2(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      dconstTyped2(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstTyped3(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstTyped3(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstTyped4(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstTyped4(floats(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BfconstTyped5(bh: Blackhole): Unit = {
    var i = 0
    while (i < floats.length) {
      fconstTyped5(floats(i), bh)
      i += 1
    }
  }

  final def fconstOrig(cst: Float, bh: Blackhole) {
    val bits: Int = java.lang.Float.floatToIntBits(cst)
    if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
      bh.consume(FCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def dconstOrig(cst: Double, bh: Blackhole) {
    val bits: Long = java.lang.Double.doubleToLongBits(cst)
    if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
      bh.consume(DCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def fconstRaw(cst: Float, bh: Blackhole) {
    val bits: Int = java.lang.Float.floatToRawIntBits(cst)
    if (bits == 0L || bits == 0x3f800000 || bits == 0x40000000) { // 0..2
      bh.consume(FCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def dconstRaw(cst: Double, bh: Blackhole) {
    val bits: Long = java.lang.Double.doubleToRawLongBits(cst)
    if (bits == 0L || bits == 0x3ff0000000000000L) { // +0.0d and 1.0d
      bh.consume(DCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def fconstTyped(cst: Float, bh: Blackhole) {
    if (cst == 0F || cst == 1F || cst == 2F) { // 0..2
      bh.consume(FCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def dconstTyped(cst: Double, bh: Blackhole) {
    if (cst == 0D || cst == 1D) { // +0.0d and 1.0d
      bh.consume(DCONST_0 + cst.asInstanceOf[Int])
    } else {
      bh.consume(cst)
    }
  }

  final def fconstTyped2(cst: Float, bh: Blackhole) {
    if (cst == 0F)
      bh.consume(FCONST_0)
    else if (cst == 1F)
      bh.consume(FCONST_1)
    else if (cst == 2F)
      bh.consume(FCONST_2)
    else
      bh.consume(cst)
  }

  final def dconstTyped2(cst: Double, bh: Blackhole) {
    if (cst == 0D)
      bh.consume(DCONST_0)
    else if (cst == 1D)
      bh.consume(DCONST_1)
    else
      bh.consume(cst)
  }

  final def fconstTyped3(cst: Float, bh: Blackhole) {
    if (cst >= 1F) {
      if (cst == 1F)
        bh.consume(FCONST_1)
      else if (cst == 2F)
        bh.consume(FCONST_2)
      else
        bh.consume(cst)
    } else if (cst == 0F)
      bh.consume(FCONST_0)
    else
      bh.consume(cst)
  }

  final def fconstTyped4(cst: Float, bh: Blackhole) {
    if (cst >= 0F && cst <= 2F) {
      if (cst == 0F)
        bh.consume(FCONST_0)
      else if (cst == 1F)
        bh.consume(FCONST_1)
      else if (cst == 2F)
        bh.consume(FCONST_2)
      else
        bh.consume(cst)
    }
    else
      bh.consume(cst)
  }

  final def fconstTyped5(cst: Float, bh: Blackhole) {
    if ((java.lang.Float.floatToRawIntBits(cst) & 0x7FFFFF) == 0)
      if (cst == 0F)
        bh.consume(FCONST_0)
      else if (cst == 1F)
        bh.consume(FCONST_1)
      else if (cst == 2F)
        bh.consume(FCONST_2)
      else
        bh.consume(cst)
    else
      bh.consume(cst)
  }
}


@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DeConstBooleanBenchmark extends DeConstIntBenchmarkBase {

  @Param(Array("true", "false", "Random"))
  var num: String = _
  var bools: Array[Boolean] = _

  @Setup(Level.Trial) def setup(): Unit = {
    val size = 1000
    val bfn = num match {
      case "Random" =>
        val r = new Random()
        (_: Int) => r.nextBoolean()
      case x =>
        val v = x.toBoolean
        (_: Int) => v
    }
    bools = Array.tabulate(1000)(bfn)
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BboolconstOrig(bh: Blackhole): Unit = {
    var i = 0
    while (i < bools.length) {
      boolconstOrig(bools(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BboolconstNew(bh: Blackhole): Unit = {
    var i = 0
    while (i < bools.length) {
      boolconstNew(bools(i), bh)
      i += 1
    }
  }

}

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class DeConstIntBenchmark extends DeConstIntBenchmarkBase {

  @Param(Array("0", "-1", "1", "-2", "2", "-4", "4", "8", "-64", "64", "-128", "128", "16384", "32768", "65536", "Random"))
  var num: String = _
  var ints: Array[Int] = _

  @Setup(Level.Trial) def setup(): Unit = {
    val size = 1000
    val ifn = num match {
      case "Random" =>
        val r = new Random()
        (_: Int) => r.nextInt()
      case x =>
        val v = x.toInt
        (_: Int) => v
    }
    ints = Array.tabulate(1000)(ifn)
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BiconstOrig(bh: Blackhole): Unit = {
    var i = 0
    while (i < ints.length) {
      iconstOrig(ints(i), bh)
      i += 1
    }
  }

  @OperationsPerInvocation(1000)
  @Benchmark
  def BboolconstNew(bh: Blackhole): Unit = {
    var i = 0
    while (i < ints.length) {
      iconstNew(ints(i), bh)
      i += 1
    }
  }
}

class DeConstIntBenchmarkBase {
  def ICONST_0 = 0

  def ICONST_1 = 1

  def BIPUSH = 1

  def SIPUSH = 1


  final def boolconstOrig(b: Boolean, bh: Blackhole) {
    iconstOrig(if (b) 1 else 0, bh)
  }

  final def iconstOrig(cst: Int, bh: Blackhole) {
    if (cst >= -1 && cst <= 5) {
      bh.consume(ICONST_0 + cst)
    } else if (cst >= java.lang.Byte.MIN_VALUE && cst <= java.lang.Byte.MAX_VALUE) {
      bh.consume(BIPUSH)
    } else if (cst >= java.lang.Short.MIN_VALUE && cst <= java.lang.Short.MAX_VALUE) {
      bh.consume(SIPUSH)
    } else {
      bh.consume(cst)
    }
  }

  final def boolconstNew(b: Boolean, bh: Blackhole) {
    if (b) bh.consume(ICONST_1)
    else bh.consume(ICONST_0)
  }

  final def iconstNew(cst: Int, bh: Blackhole) {
    if ((cst & 0xFF) == cst) {
      if (cst >= -1 && cst <= 5) {
        bh.consume(ICONST_0 + cst)
      } else
        bh.consume(BIPUSH)
    } else if ((cst & 0xFFFF) == cst) {
      bh.consume(SIPUSH)
    } else {
      bh.consume(cst)
    }
  }


}


