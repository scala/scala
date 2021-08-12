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

package scala.collection.mutable

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 15)
@Measurement(iterations = 15)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ArrayBufferBenchmark {
  @Param(Array(/*"0", "1",*/ "10", "100", "1000", "10000"))
  var size: Int = _

  var ref: ArrayBuffer[Int] = _

  @Setup(Level.Trial) def init: Unit = {
    ref = new ArrayBuffer
    for(i <- 0 until size) ref += i
  }

  @Benchmark def filterInPlace(bh: Blackhole): Unit = {
    val b = ref.clone()
    b.filterInPlace(_ % 2 == 0)
    bh.consume(b)
  }

  @Benchmark def update(bh: Blackhole): Unit = {
    val b = ref.clone()
    var i = 0
    while(i < size) {
      b.update(i, -1)
      i += 2
    }
    bh.consume(b)
  }

  @Benchmark def addAll(bh: Blackhole): Unit = {
    val b1 = ref.clone()
    val b2 = ref.clone()
    var i = 0
    b1.addAll(b2)
    bh.consume(b1)
  }

  @Benchmark def flatMapInPlace1(bh: Blackhole): Unit = {
    val b = ref.clone()
    val seq = Seq(0,0)
    b.flatMapInPlace { _ => seq }
    bh.consume(b)
  }

  @Benchmark def iteratorA(bh: Blackhole): Unit = {
    val b = ref.clone()
    var n = 0
    for (x <- b.iterator) n += x
    bh.consume(n)
    bh.consume(b)
  }

  @Benchmark def iteratorB(bh: Blackhole): Unit = {
    val b = ref.clone()
    bh.consume(b.iterator.toVector)
    bh.consume(b)
  }

  @Benchmark def reverseIteratorA(bh: Blackhole): Unit = {
    val b = ref.clone()
    var n = 0
    for (x <- b.reverseIterator) n += x
    bh.consume(n)
    bh.consume(b)
  }

  @Benchmark def reverseIteratorB(bh: Blackhole): Unit = {
    val b = ref.clone()
    bh.consume(b.reverseIterator.toVector)
    bh.consume(b)
  }

}
