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

package scala.collection
package mutable

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
class ConstructionBenchmark {
  @Param(Array("0", "1", "10", "100"))
  var size: Int = _

  var values: Range = _

  @Setup(Level.Trial) def init(): Unit = {
    values = 1 to size
  }

  @Benchmark def listBuffer_new: Any = {
    new ListBuffer ++= values
  }

  @Benchmark def listBuffer_from: Any = {
    ListBuffer from values
  }

  @Benchmark def listBuffer_to: Any = {
    values to ListBuffer
  }
}
