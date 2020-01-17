package scala.collection.immutable

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra._
import org.openjdk.jmh.runner.IterationType
import benchmark._
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.AverageTime))
@Fork(1)
@Threads(1)
@Warmup(iterations = 8)
@Measurement(iterations = 8)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
class VectorMapBenchmark {
  @Param(Array ("5", "10", "100", "1000", "1000000"))
  var size: Int = _

  var kvs: Seq[(Int, Int)] = _
  var vm: VectorMap[Int, Int] = _
  var lhm: LinkedHashMap[Int, Int] = _

  @Setup(Level.Trial)
  def initKeys(): Unit = {
    val unique = (0 to size).map(i => i -> i)
    kvs = unique ++ unique
    vm = VectorMap.from(kvs)
    lhm = LinkedHashMap.from(kvs)
//    println("arity=" + LinkedHashMap.arity)
  }

//  @Benchmark
//  def buildVm(bh: Blackhole): Unit = {
//    bh.consume(VectorMap.from(kvs))
//  }
//  @Benchmark
//  def buildLhm(bh: Blackhole): Unit = {
//    bh.consume(LinkedHashMap.from(kvs))
//  }

//  @Benchmark
//  def foreachVm(bh: Blackhole): Unit = {
//    vm.foreach(bh.consume)
//  }
  @Benchmark
  def foreachLhm(bh: Blackhole): Unit = {
    lhm.foreach(bh.consume)
  }
  /*
  @Benchmark
  def getVm(bh: Blackhole): Unit = {
    bh.consume(vm.get(size / 2))
    bh.consume(vm.get(size))
  }
  @Benchmark
  def getLhm(bh: Blackhole): Unit = {
    bh.consume(lhm.get(size / 2))
    bh.consume(lhm.get(size))
  }
  */
//  @Benchmark
//  def updatedVm(bh: Blackhole): Unit = {
//    bh.consume(vm.updated(size / 2, -1))
////    bh.consume(vm.updated(size, -1))
//  }
//  @Benchmark
//  def updatedLhm(bh: Blackhole): Unit = {
//    bh.consume(lhm.updated(size / 2, -1))
////    bh.consume(lhm.updated(size, -1))
//  }
  /*

  @Benchmark
  def removedVm(bh: Blackhole): Unit = {
    bh.consume(vm.removed(size/2))
  }
  @Benchmark
  def removedLhm(bh: Blackhole): Unit = {
    bh.consume(lhm.removed(size/2))
  }

   */
}
