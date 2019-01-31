package scala.reflect

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ClassTagBenchmark {
  var unitClassTag: ClassTag[_] = null
  var booleanClassTag: ClassTag[_] = null
  var byteClassTag: ClassTag[_] = null
  var shortClassTag: ClassTag[_] = null
  var charClassTag: ClassTag[_] = null
  var intClassTag: ClassTag[_] = null
  var longClassTag: ClassTag[_] = null
  var floatClassTag: ClassTag[_] = null
  var doubleClassTag: ClassTag[_] = null
  var refClassTag: ClassTag[_] = null
  var otherValue: Object = null
  var arraySize: Int = 100

  @Setup def setup(): Unit = {
    unitClassTag = classTag[Unit]
    booleanClassTag = classTag[Boolean]
    byteClassTag = classTag[Byte]
    shortClassTag = classTag[Short]
    charClassTag = classTag[Char]
    intClassTag = classTag[Int]
    longClassTag = classTag[Long]
    floatClassTag = classTag[Float]
    doubleClassTag = classTag[Double]
    refClassTag = classTag[ClassTagBenchmark]
    otherValue = new Object
  }

  @Benchmark def primitivesNegOnRefClassTag(bh: Blackhole): Any = {
    bh.consume(refClassTag.unapply(()))
    bh.consume(refClassTag.unapply(1: Byte))
    bh.consume(refClassTag.unapply('A'))
    bh.consume(refClassTag.unapply(1: Short))
    bh.consume(refClassTag.unapply(1))
    bh.consume(refClassTag.unapply(1L))
    bh.consume(refClassTag.unapply(1f))
    bh.consume(refClassTag.unapply(1d))
  }

  @Benchmark def primitivesPos(bh: Blackhole): Any = {
    bh.consume(unitClassTag.unapply(()))
    bh.consume(booleanClassTag.unapply(true))
    bh.consume(byteClassTag.unapply(1: Byte))
    bh.consume(charClassTag.unapply('A'))
    bh.consume(shortClassTag.unapply(1: Short))
    bh.consume(intClassTag.unapply(1))
    bh.consume(longClassTag.unapply(1L))
    bh.consume(floatClassTag.unapply(1f))
    bh.consume(doubleClassTag.unapply(1d))
  }

  @Benchmark def primitivesNewArray(bh: Blackhole): Any = {
    bh.consume(unitClassTag.newArray(arraySize))
    bh.consume(booleanClassTag.newArray(arraySize))
    bh.consume(charClassTag.newArray(arraySize))
    bh.consume(shortClassTag.newArray(arraySize))
    bh.consume(intClassTag.newArray(arraySize))
    bh.consume(longClassTag.newArray(arraySize))
    bh.consume(floatClassTag.newArray(arraySize))
    bh.consume(doubleClassTag.newArray(arraySize))
  }

  @Benchmark def refClassTagNewArray(bh: Blackhole): Any = {
    bh.consume(refClassTag.newArray(arraySize))
  }

  @Benchmark def doubleClassTagNewArray(bh: Blackhole): Any = {
    bh.consume(doubleClassTag.newArray(arraySize))
  }

  @Benchmark def refClassTagUnapplyNeg2(bh: Blackhole): Any = refClassTag.unapply(otherValue)

  @Benchmark def refClassTagUnapplyNeg2Direct(bh: Blackhole): Any = unapplyDirect(refClassTag, otherValue)

  def unapplyDirect(ct: ClassTag[_], x: AnyRef): Option[_] = {
    if (null != x && (ct.runtimeClass.isInstance(x))) Some(x)
    else None
  }
}
