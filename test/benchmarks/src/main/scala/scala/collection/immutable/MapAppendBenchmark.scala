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

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class MapAppendBenchmark {

  @Param(Array("10", "100", "1000"))
  var size: Int = _

  @Benchmark def plus1(bh: Blackhole): Unit = {
    var m = Map.empty[Int, Unit]
    var i = 0
    while(i < size) {
      m = m + ((i -> ())) + (((i+size) -> ()))
      i += 1
    }
    bh.consume(m)
  }

  @Benchmark def plus2(bh: Blackhole): Unit = {
    var m = Map.empty[Int, Unit]
    var i = 0
    while(i < size) {
      m = m + ((i -> ()), ((i+size) -> ()))
      i += 1
    }
    bh.consume(m)
  }

  @Benchmark def plus2Empty(bh: Blackhole): Unit = {
    val empty = IndexedSeq.empty
    var m = Map.empty[Int, Unit]
    var i = 0
    while(i < size) {
      m = m + ((i -> ()), ((i+size) -> ()), empty: _*)
      i += 1
    }
    bh.consume(m)
  }
}
