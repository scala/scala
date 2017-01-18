package bench

import strawman.collection.immutable.{LazyList, List}

import scala.{Any, AnyRef, App, Int, Long}
import scala.Predef.{println, ArrowAssoc}
import scala.compat.Platform
import java.lang.Runtime

import strawman.collection.mutable.{ArrayBuffer, ListBuffer}

object MemoryFootprint extends App {

  val sizes = scala.List(8, 64, 512, 4096, 32768, 262144, 2097152)

  val runtime = Runtime.getRuntime
  val obj: AnyRef = null
  var placeholder: Any = _

  def benchmark[A](gen: Int => A): scala.List[(Int, Long)] = (
    // We run 5 iterations and pick the last result only
    for (_ <- scala.Range(0, 5)) yield {
      for (size <- sizes) yield {
        placeholder = null
        Platform.collectGarbage()
        val memBefore = runtime.totalMemory() - runtime.freeMemory()
        placeholder = gen(size)
        Platform.collectGarbage()
        val memAfter = runtime.totalMemory() - runtime.freeMemory()
        size -> (memAfter - memBefore)
      }
    }
  ).last

  val memories =
    scala.Predef.Map(
      "scala.List"  -> benchmark(scala.List.fill(_)(obj)),
      "List"        -> benchmark(List.fill(_)(obj)),
      "LazyList"    -> benchmark(LazyList.fill(_)(obj)),
      "ArrayBuffer" -> benchmark(ArrayBuffer.fill(_)(obj)),
      "ListBuffer"  -> benchmark(ListBuffer.fill(_)(obj))
    )

  // Print the results as a CSV document
  println("Collection;" + sizes.mkString(";"))
  for ((name, values) <- memories) {
    println(name + ";" + values.map(_._2).mkString(";"))
  }

}
