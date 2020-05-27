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


import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Level, Measurement, Mode, OutputTimeUnit, Param, Scope, Setup, State, Threads, Warmup}
import org.openjdk.jmh.infra.Blackhole
import scala.collection.mutable.ArrayBuffer


@BenchmarkMode(Array(Mode.AverageTime))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class GroupByBenchmark {
  @Param(Array("128", "512", "2048", "8192"))
  var size        : Int = _

  @Param(Array("0", "32"))
  var hashCodeCost: Int = _

  @Param(Array("8", "2147483647"))
  var maxNumGroups: Int = _

  private case class Key(a: Int) {
    override def hashCode(): Int = {
      Blackhole.consumeCPU(hashCodeCost)
      Integer.hashCode(a)
    }
  }

  private case class Groupable(a: Int) {
    val key = new Key(a % maxNumGroups)
  }

  private var groupables: ArrayBuffer[Groupable] = _

  private class GroupByWrapper[A](as: collection.Iterable[A]) extends collection.Iterable[A] {
    override def iterator: Iterator[A] = as.iterator
    override protected def newBuilder = new mutable.Builder[A, Iterable[A]] {
      override def clear(): Unit = ()
      override def result(): Iterable[A] = Nil
      override def +=(elem: A): this.type = this
    }
  }

  @Setup(Level.Trial) def setup(): Unit = {
    groupables = ArrayBuffer.tabulate(size)(Groupable(_))
  }

  @Benchmark def buildArrayBuffer(): AnyRef = {
    groupBy(groupables)
  }
  @Benchmark def buildNil(): AnyRef = {
    groupBy(new GroupByWrapper[Groupable](groupables))
  }

  private def groupBy[B](as: collection.Iterable[Groupable]) = as.groupBy(_.key)
}
