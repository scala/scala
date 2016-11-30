package scala.collection

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class BitManipulationBenchmark {
  val powersOfTwo = Array(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824)

  //////////////////////////////////////////////

  @Benchmark def withIntegerBitCount(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withIntegerBitCount(v)
      // assert (leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  private def withIntegerBitCount(v: Int) = Integer.SIZE - Integer.bitCount(v - 1)

  //////////////////////////////////////////////

  @Benchmark def withIntegerNumberOfLeadingZeros(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withIntegerNumberOfLeadingZeros(v)
      // assert (leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  private def withIntegerNumberOfLeadingZeros(v: Int) = Integer.numberOfLeadingZeros(v - 1)

  //////////////////////////////////////////////

  @Benchmark def withLoop(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withLoop(v)
      bh.consume(leadingZeros)
    }
  }

  private def withLoop(v: Int): Int = {
    var r = Integer.SIZE
    var copy = v >> 1
    while (copy != 0) {
      r -= 1
      copy = copy >> 1
    }
    r
  }

  //////////////////////////////////////////////

  @Benchmark def withMatch(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withMatch(v)
      // assert (leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  private def withMatch(i: Int) = i match {
    case 1 => 32
    case 2 => 31
    case 4 => 30
    case 8 => 29
    case 16 => 28
    case 32 => 27
    case 64 => 26
    case 128 => 25
    case 256 => 24
    case 512 => 23
    case 1024 => 22
    case 2048 => 21
    case 4096 => 20
    case 8192 => 19
    case 16384 => 18
    case 32768 => 17
    case 65536 => 16
    case 131072 => 15
    case 262144 => 14
    case 524288 => 13
    case 1048576 => 12
    case 2097152 => 11
    case 4194304 => 10
    case 8388608 => 9
    case 16777216 => 8
    case 33554432 => 7
    case 67108864 => 6
    case 134217728 => 5
    case 268435456 => 4
    case 536870912 => 3
    case 1073741824 => 2
  }


  //////////////////////////////////////////////

  @Benchmark def with2DeBruijn(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = with2DeBruijn(v)
      // assert (leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  // https://graphics.stanford.edu/~seander/bithacks.html#IntegerLogDeBruijn
  private val multiplyDeBruijnBitPosition2 = Array(32, 31, 4, 30, 3, 18, 8, 29, 2, 10, 12, 17, 7, 15, 28, 24, 1, 5, 19, 9, 11, 13, 16, 25, 6, 20, 14, 26, 21, 27, 22, 23)

  private def with2DeBruijn(v: Int) = multiplyDeBruijnBitPosition2((v * 0x077CB531) >>> 27)


  //////////////////////////////////////////////

  @Benchmark def withBinSearch(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withBinSearch(v)
      // assert (leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  private def withBinSearch(v: Int) =
    if (v < 65536) if (v < 256) if (v < 16) if (v < 4) if (v == 1) 32 else 31
    else if (v == 4) 30 else 29
    else if (v < 64) if (v == 16) 28 else 27
    else if (v == 64) 26 else 25
    else if (v < 4096) if (v < 1024) if (v == 256) 24 else 23
    else if (v == 1024) 22 else 21
    else if (v < 16384) if (v == 4096) 20 else 19
    else if (v == 16384) 18 else 17
    else if (v < 16777216) if (v < 1048576) if (v < 262144) if (v == 65536) 16 else 15
    else if (v == 262144) 14 else 13
    else if (v < 4194304) if (v == 1048576) 12 else 11
    else if (v == 4194304) 10 else 9
    else if (v < 268435456) if (v < 67108864) if (v == 16777216) 8 else 7
    else if (v == 67108864) 6 else 5
    else if (v < 1073741824) if (v == 268435456) 4 else 3
    else if (v == 1073741824) 2 else 1

  //////////////////////////////////////////////

  @Benchmark def withSumBinSearch(bh: Blackhole) {
    for (v <- powersOfTwo) {
      val leadingZeros = withSumBinSearch(v)
      // assert(leadingZeros == withLoop(v), s"$leadingZeros != ${withLoop(v)} ($v)")
      bh.consume(leadingZeros)
    }
  }

  private def withSumBinSearch(v: Int): Int = {
    var exponent = Integer.SIZE
    var remaining = v
    if (remaining >= 65536) { remaining >>>= 16; exponent = 16 }
    if (remaining >= 256) { remaining >>>= 8; exponent -= 8 }
    if (remaining >= 16) { remaining >>>= 4; exponent -= 4 }
    if (remaining >= 4) { remaining >>>= 2; exponent -= 2 }
    if (remaining >= 2) exponent - 1 else exponent
  }
}