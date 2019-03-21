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
@Warmup(iterations = 20)
@Measurement(iterations = 20)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class ListBufferBenchmark {
  @Param(Array(/*"0", "1",*/ "10", "100", "1000", "10000"))
  var size: Int = _

  var ref: ListBuffer[Int] = _

  @Setup(Level.Trial) def init: Unit = {
    ref = new ListBuffer
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

  @Benchmark def remove1(bh: Blackhole): Unit = {
    val b = ref.clone()
    var i = 0
    while(i < size/2) {
      b.remove(i)
      i += 2
    }
    bh.consume(b)
  }

  @Benchmark def remove2(bh: Blackhole): Unit = {
    val b = ref.clone()
    var i = 0
    while(i < size/4) {
      b.remove(i, 2)
      i += 2
    }
    bh.consume(b)
  }

  @Benchmark def insert(bh: Blackhole): Unit = {
    val b = ref.clone()
    var i = 0
    while(i < size) {
      b.insert(i, 0)
      i += 2
    }
    bh.consume(b)
  }

  @Benchmark def insertAll(bh: Blackhole): Unit = {
    val b = ref.clone()
    val seq = Seq(0,0)
    var i = 0
    while(i < size/2) {
      b.insertAll(i, seq)
      i += 4
    }
    bh.consume(b)
  }

  @Benchmark def flatMapInPlace1(bh: Blackhole): Unit = {
    val b = ref.clone()
    val seq = Seq(0,0)
    b.flatMapInPlace { _ => seq }
    bh.consume(b)
  }
}
