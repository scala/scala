package scala.collection

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class StringOpsBenchmark {
  @Param(Array("0", "1", "10", "100", "1000"))
  var size: Int = _

  var prefix: String = _
  var suffix: String = _

  // Used when we want to test stripPrefix etc with a string which is definately not present
  var impossibleString: String = _

  var testObject: StringOps = _

  @Setup(Level.Trial) def initKeys(): Unit = {
    val randomString = Random.nextString(size)

    testObject = new StringOps(randomString)

    val prefixAndSuffixLength = size / 10
    prefix = randomString.substring(0, prefixAndSuffixLength)
    suffix = randomString.reverse.substring(0, prefixAndSuffixLength)

    impossibleString = randomString * 2
  }

  @Benchmark def map: Any = {
    testObject.map(char => char)
  }

  @Benchmark def flatMap: Any = {
    testObject.flatMap(char => char.toString)
  }

  @Benchmark def concat: Any = {
    testObject.concat(suffix)
  }

  @Benchmark def padTo: Any = {
    testObject.padTo(size * 2, 'x')
  }

  @Benchmark def prepended: Any = {
    testObject.prepended('x')
  }

  @Benchmark def appended: Any = {
    testObject.appended('x')
  }

  @Benchmark def patch_zeroElementsfromZero: Any = {
    testObject.patch(0, suffix, 0)
  }

  @Benchmark def patch_fifteenElementsfromZero: Any = {
    testObject.patch(0, suffix, 15)
  }

  @Benchmark def patch_fiftyElementsfromZero: Any = {
    testObject.patch(0, suffix, 50)
  }

  @Benchmark def patch_zeroElementsfromFifteen: Any = {
    testObject.patch(15, suffix, 0)
  }

  @Benchmark def patch_fifteenElementsfromFifty: Any = {
    testObject.patch(50, suffix, 15)
  }

  @Benchmark def patch_fiftyElementsfromFifty: Any = {
    testObject.patch(50, suffix, 50)
  }

  @Benchmark def updated_atZero: Any = {
    testObject.updated(0, 'x')
  }

  @Benchmark def slice: Any = {
    testObject.slice(size / 10, size / 2)
  }

  @Benchmark def *(): Any = {
    testObject * 10
  }

  @Benchmark def stripLineEnd: Any = {
    testObject.stripLineEnd
  }

  @Benchmark def linesWithSeparators: Any = {
    testObject.linesWithSeparators
  }

  @Benchmark def lines: Any = {
    testObject.linesIterator
  }

  @Benchmark def capitalize: Any = {
    testObject.capitalize
  }

  @Benchmark def stripPrefix_present: Any = {
    testObject.stripPrefix(prefix)
  }

  @Benchmark def stripPrefix_notPresent: Any = {
    testObject.stripPrefix(impossibleString)
  }

  @Benchmark def stripSuffix_present: Any = {
    testObject.stripSuffix(suffix)
  }

  @Benchmark def stripSuffix_notPresent: Any = {
    testObject.stripSuffix(impossibleString)
  }

  @Benchmark def replaceAllLiterally: Any = {
    testObject.replaceAllLiterally("A", "B")
  }

  @Benchmark def stripMargin: Any = {
    testObject.stripMargin
  }

  @Benchmark def split: Any = {
    testObject.split('A')
  }
}
