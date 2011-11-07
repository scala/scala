package scala.collection.parallel.benchmarks.parallel_array


import scala.collection.parallel.benchmarks._
import scala.collection.parallel.mutable.ParArray
import extra166y.{ParallelArray => JSR166Array}


object ReduceLight extends Companion {
  def benchName = "reduce-light";
  def apply(sz: Int, parallelism: Int, what: String) = new ReduceLight(sz, parallelism, what)
  override def comparisons = List("jsr")
  override def defaultSize = 200000
}


class ReduceLight(sz: Int, p: Int, what: String)
extends Resettable[Cont](sz, p, what, new Cont(_), new Array[Any](_), classOf[Cont]) {
  def companion = ReduceLight
  override def repetitionsPerRun = 350
  override val runs = 20
  
  def runpar = {
    pa.reduce(Cont.op)
//    updatePar
  }
  
  def runjsr = {
    jsrarr.reduce(Cont.reducer, new Cont(0))
//    updateJsr
  }
  
  def runseq = {
    sequentialReduce(Cont.op, sz, new Cont(0))
//    updateSeq
  }
  
  override def comparisonMap = collection.Map("jsr" -> runjsr _)
  
}










