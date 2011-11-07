package scala.collection.parallel.benchmarks.arrays


import scala.collection.parallel.benchmarks._


abstract class Resetting[T: Manifest](elemcreate: Int => T, sz: Int, p: Int, what: String)
extends Bench {
  val size = sz
  val parallelism = p
  val runWhat = what
  
  var anyarray: Array[Any] = null
  var castarray: AnyRef = null
  var gencastarray: Array[T] = null
  var manifarray: Array[T] = null
  
  reset
  
  def reset = what match {
    case "any" =>
      anyarray = new Array[Any](sz)
      for (i <- 0 until sz) anyarray(i) = elemcreate(i)
    case "cast" =>
      val arr = new Array[T](sz)
      for (i <- 0 until sz) arr(i) = elemcreate(i)
      castarray = arr
    case "gencast" =>
      gencastarray = new Array[T](sz)
      for (i <- 0 until sz) gencastarray(i) = elemcreate(i)
    case "manif" =>
      manifarray = new Array[T](sz)
      for (i <- 0 until sz) manifarray(i) = elemcreate(i)
    case "unknown" =>
      manifarray = new Array[T](sz)
      for (i <- 0 until sz) manifarray(i) = elemcreate(i)
    case _ =>
  }
}
