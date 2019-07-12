package scala.collection
package mutable

import java.util.concurrent.TimeUnit

import scala.util.Random

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Benchmark)
class ArrayDequeBenchmark {

  /** Initial size of array. */
  @Param(Array("0", "1000")) // Zero-length is empty array
  var length: Int = _

  var arr: ArrayDeque[Int] = _
  var fraction: Int = _
  var near: Int = _
  var chunk: Int = _
  var tail: Int = _
  var tailChunk: Int = _
  var nearTail: Int = _
  var nearTailChunk: Int = _

  /** Number of values to add, insert, prepend, access, ... */
  @Param(Array("1000"))
  var range: Int = _

  var rangeArr: Array[Int] = _

  @Setup(Level.Trial)
  def initRange: Unit = {
    rangeArr = (1 to range.toInt).toArray
  }

  @Setup(Level.Invocation)
  def initArray: Unit = {
    arr = ArrayDeque.range(0, length)
    fraction = 20
    near = length / fraction
    chunk = Math.min(arr.size, fraction)
    tail = Math.max(0, arr.size - 1)
    tailChunk = Math.max(0, tail - chunk)
    nearTail = Math.max(0, tail - near)
    nearTailChunk = Math.max(0, nearTail - chunk)
  }

  @Benchmark
  def addAll(bh: Blackhole): Unit = {
    bh.consume(arr ++= rangeArr)
  }

  @Benchmark
  def prependAll(bh: Blackhole): Unit = {
    bh.consume(arr.++=:(rangeArr))
  }

  @Benchmark
  def removeFromHead(bh: Blackhole): Unit = {
    arr.remove(0, chunk)
    bh.consume(arr)
  }

  @Benchmark
  def removeFromTail(bh: Blackhole): Unit = {
    arr.remove(tailChunk, chunk)
    bh.consume(arr)
  }

  @Benchmark
  def removeNearHead(bh: Blackhole): Unit = {
    arr.remove(near, chunk)
    bh.consume(arr)
  }

  @Benchmark
  def removeNearTail(bh: Blackhole): Unit = {
    arr.remove(nearTailChunk, chunk)
    bh.consume(arr)
  }

  @Benchmark
  def add(bh: Blackhole): Unit = {
    rangeArr.foreach {
      arr += _
    }
    bh.consume(arr)
  }

  @Benchmark
  def prepend(bh: Blackhole): Unit = {
    rangeArr.foreach {
      arr.+=:(_)
    }
    bh.consume(arr)
  }

  @Benchmark
  def insertAllNearHead(bh: Blackhole): Unit = {
    bh.consume(arr.insertAll(near, rangeArr))
  }

  @Benchmark
  def insertAllHead(bh: Blackhole): Unit = {
    bh.consume(arr.insertAll(0, rangeArr))
  }

  @Benchmark
  def insertAllNearTail(bh: Blackhole): Unit = {
    bh.consume(arr.insertAll(nearTail, rangeArr))
  }

  @Benchmark
  def insertAllTail(bh: Blackhole): Unit = {
    bh.consume(arr.insertAll(tail, rangeArr))
  }

  @Benchmark
  def reverse(bh: Blackhole): Unit = {
    bh.consume(arr.reverse)
  }

  @Benchmark
  def _get(bh: Blackhole): Unit = {
    rangeArr.foreach { i =>
      if (arr.isDefinedAt(i))
        bh.consume(arr(i))
    }
  }

  @Benchmark
  def toArray(bh: Blackhole): Unit = {
    bh.consume(arr.toArray)
  }

  @Benchmark
  def clear(bh: Blackhole): Unit = {
    arr.clear()
    bh.consume(arr)
  }
}
