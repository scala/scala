// Concurrent use of type operations in the runtime reflection universe must not
// corrupt state shared between threads.

import java.util.concurrent.{CyclicBarrier, Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

trait A { def a: Int }
trait B { def b: Int }
class D { def dep(x: AnyRef): x.type = x; def nonDep(x: AnyRef): Map[List[Option[Int]], Map[String, List[(Int, String)]]] = null }

object Test {
  val u = scala.reflect.runtime.universe.asInstanceOf[scala.reflect.runtime.JavaUniverse]
  import u._

  final val Threads = 8
  final val Iterations = 4000

  /** Run `body(threadIndex)` `Iterations` times on each of `Threads` threads; return the number of times it returned false. */
  def stress(name: String)(body: Int => Boolean): Unit = {
    val pool = Executors.newFixedThreadPool(Threads)
    val barrier = new CyclicBarrier(Threads)
    val bad = new AtomicInteger
    val errors = new AtomicInteger
    for (t <- 0 until Threads) pool.execute { () =>
      barrier.await()
      var i = 0
      while (i < Iterations) {
        try if (!body(t)) bad.incrementAndGet()
        catch { case _: Throwable => errors.incrementAndGet() }
        i += 1
      }
    }
    pool.shutdown()
    Predef.assert(pool.awaitTermination(5, TimeUnit.MINUTES))
    println(s"$name: wrong=${bad.get} exceptions=${errors.get}")
  }

  def main(args: Array[String]): Unit = {
    // GlbLubs.globalGlbDepth and the collector in maxTypes
    val glbTs = List(typeOf[A { def x: Int }], typeOf[B { def y: Int }])
    val expectedGlb = glb(glbTs).toString
    stress("glb")(_ => glb(glbTs).toString == expectedGlb)
    // A corrupted global depth would make every later glb drop refinement members.
    println(s"glb after: ${glb(glbTs)}")

    // Type.isErroneous
    val errorTp = appliedType(typeOf[Map[_, _]].typeConstructor, List(typeOf[List[Int]], ErrorType))
    val okTp = typeOf[Map[List[Option[Int]], Map[String, List[(Int, String)]]]]
    Predef.assert(errorTp.isErroneous && !okTp.isErroneous)
    stress("isErroneous")(t => if (t % 2 == 0) errorTp.isErroneous else !okTp.isErroneous)

    // MethodType.isDependentMethodType
    def methodTypeFactory(name: String): () => Type = {
      val m = typeOf[D].decl(TermName(name))
      val (params, res) = (m.paramLists.head, m.info.resultType)
      () => MethodType(params, res) // fresh, as `isDependentMethodType` is cached
    }
    val dep = methodTypeFactory("dep")
    val nonDep = methodTypeFactory("nonDep")
    Predef.assert(dep().isDependentMethodType && !nonDep().isDependentMethodType)
    stress("isDependentMethodType")(t => if (t % 2 == 0) dep().isDependentMethodType else !nonDep().isDependentMethodType)
  }
}
