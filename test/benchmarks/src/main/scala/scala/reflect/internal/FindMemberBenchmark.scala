package scala.reflect.internal

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.tools.nsc.{Global, Settings}

@BenchmarkMode(Array(org.openjdk.jmh.annotations.Mode.Throughput))
@Fork(2)
@Threads(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@OutputTimeUnit(TimeUnit.SECONDS)
@State(Scope.Benchmark)
class FindMemberBenchmark {
  type G <: Global with Singleton
  var Type_List: G#Type = _
  var Type_ListRefined: G#Type = _
  var Name_Blerg: G#TermName = _
  var Name_toString: G#TermName = _
  var Name_isEmpty: G#TermName = _

  @Setup(Level.Trial) def setup(): Unit = {
    val settings = new Settings()
    settings.usejavacp.value = true
    settings.stopAfter.value = List("typer")
    val global = new Global(settings).asInstanceOf[G]
    import global._
    new Run()
    Type_List = typeOf[List[_]]
    Type_ListRefined = typeOf[List[_] with String { def foo: Int }] // this sort of type turns up in LUBs (search "val lubRefined = ")
    Name_Blerg = TermName("Blerg")
    Name_toString = TermName("toString")
    Name_isEmpty = TermName("isEmpty")

  }

  @Benchmark
  def findMember(bh: Blackhole): Unit = {
    bh.consume(Type_List.member(Name_Blerg))
    bh.consume(Type_List.member(Name_isEmpty))
    bh.consume(Type_List.member(Name_toString))
  }

  @Benchmark
  def findMemberRefined(bh: Blackhole): Unit = {
    bh.consume(Type_ListRefined.member(Name_Blerg))
    bh.consume(Type_ListRefined.member(Name_isEmpty))
    bh.consume(Type_ListRefined.member(Name_toString))
  }
}
