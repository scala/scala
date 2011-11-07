package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray
import extra166y.{ParallelArray => JSR166Array}


class Cont(val in: Int) {
  var num = in
  override def toString = in.toString
}

object Cont {
  val pred = (a: Cont) => a.in > 100
  
  val predjsr = new extra166y.Ops.Predicate[Cont] {
    def op(a: Cont) = a.in > 100
  }
  
  val op = (a: Cont, b: Cont) => {
    b.num = a.in + b.in
    b
  }
  
  val opnew = (a: Cont, b: Cont) => new Cont(a.in + b.in)
  
  val opheavy = (a: Cont, b: Cont) => {
    heavyComputation(a, b)
  }
  
  val reducer = new extra166y.Ops.Reducer[Cont] {
      def op(a: Cont, b: Cont) = {
        b.num = a.in + b.in
        b
      }
    }
  
  val reducernew = new extra166y.Ops.Reducer[Cont] {
    def op(a: Cont, b: Cont) = new Cont(a.in + b.in)
  }
  
  val reducerheavy = new extra166y.Ops.Reducer[Cont] {
    def op(a: Cont, b: Cont) = heavyComputation(a, b)
  }
  
  def heavyComputation(a: Cont, b: Cont) = {
    val f = a.in
    val s = b.in
    var i = 0
    var res = f * s
    while (i < 50000) {
      if ((i + f) % 3 == 0) res += s
      else res -= f
      i += 1
    }
    b.num = res
    b
  }
}

abstract class Resettable[T](val size: Int, val parallelism: Int, val runWhat: String, 
    elemcreator: Int => T, arrcreator: Int => Array[Any], cls: Class[T])
extends Bench with SequentialOps[T] {
  val forkjoinpool = new scala.concurrent.forkjoin.ForkJoinPool(parallelism)
  forkjoinpool.setMaximumPoolSize(parallelism)
  val papool = new jsr166y.ForkJoinPool(parallelism)
  papool.setMaximumPoolSize(parallelism)
  
  var pa: ParArray[T] = null
  var jsrarr: JSR166Array[T] = null
  reset
  
  def reset = runWhat match {
    case "seq" =>
      arr = arrcreator(size)
      for (i <- 0 until size) arr(i) = elemcreator(i)
    case "par" =>
      pa = new ParArray[T](size)
      collection.parallel.tasksupport.environment = forkjoinpool
      for (i <- 0 until size) pa(i) = elemcreator(i)
    case "jsr" =>
      jsrarr  = JSR166Array.create(size, cls, papool)
      for (i <- 0 until size) jsrarr.set(i, elemcreator(i))
    case _ => throw new IllegalArgumentException("Unknown type: " + runWhat)
  }
  
  var updateCounter = 0
  def incUpdateCounter {
    updateCounter += 1
    if (updateCounter > size) updateCounter = 0
  }
  
  def updateSeq {
    val tmp = arr(updateCounter)
    arr(updateCounter) = arr(size - updateCounter - 1)
    arr(size - updateCounter - 1) = tmp
    incUpdateCounter
  }
  
  def updatePar {
    val tmp = pa(updateCounter)
    pa(updateCounter) = pa(size - updateCounter - 1)
    pa(size - updateCounter - 1) = tmp
    incUpdateCounter
  }
  
  def updateJsr {
    val tmp = jsrarr.get(updateCounter)
    jsrarr.set(updateCounter, jsrarr.get(size - updateCounter - 1))
    jsrarr.set(size - updateCounter - 1, tmp)
    incUpdateCounter
  }
  
  override def printResults {
    println(" --- Fork join pool state --- ")
    println("Parallelism: " + forkjoinpool.getParallelism)
    println("Active threads: " + forkjoinpool.getActiveThreadCount)
    println("Work stealings: "  + forkjoinpool.getStealCount)
  }
}






