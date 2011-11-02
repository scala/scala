package scala.collection.parallel


import scala.collection.mutable.LinkedHashSet

import benchmarks._


/**
 * All benchmarks are registered here.
 * 
 * @author prokopec
 */
trait BenchmarkRegister {
  
  val benchcreators = LinkedHashSet[BenchCompanion]()
  
  def register(companion: BenchCompanion) = benchcreators += companion
  
  // parallel array benchmarks
  register(parallel_array.ReduceLight)
  register(parallel_array.ReduceNew)
  register(parallel_array.ReduceList)
  register(parallel_array.ReducePrime)
  register(parallel_array.ReduceHeavy)
  register(parallel_array.CountLight)
  register(parallel_array.CountList)
  register(parallel_array.CountHeavy)
  register(parallel_array.ForeachLight)
  register(parallel_array.ForeachHeavy)
  register(parallel_array.SumLight)
  register(parallel_array.MinLight)
  register(parallel_array.MapLight)
  register(parallel_array.FilterLight)
  register(parallel_array.PartitionLight)
  register(parallel_array.PartialMapLight)
  register(parallel_array.FlatMapLight)
  register(parallel_array.PlusPlus)
  register(parallel_array.ForallLight)
  register(parallel_array.ForallQuickStop)
  register(parallel_array.ForallStop80k)
  register(parallel_array.ForallHeavy)
  register(parallel_array.ExistsLight)
  register(parallel_array.FindLight)
  register(parallel_array.TakeMany)
  register(parallel_array.DropMany)
  register(parallel_array.SliceMany)
  register(parallel_array.SliceMedium)
  register(parallel_array.SliceFew)
  register(parallel_array.SplitHalf)
  register(parallel_array.TakeWhileLight)
  register(parallel_array.SpanLight)
  register(parallel_array.CopyToArray)
  register(parallel_array.SegmentLength)
  register(parallel_array.IndexWhere)
  register(parallel_array.LastIndexWhere)
  register(parallel_array.Reverse)
  register(parallel_array.ReverseMap)
  register(parallel_array.SameElementsLong)
  register(parallel_array.Corresponds)
  register(parallel_array.DiffHalf)
  register(parallel_array.IntersectHalf)
  register(parallel_array.RemoveDuplicates)
  register(parallel_array.PatchHalf)
  register(parallel_array.PadToDouble)
  register(parallel_array.AggregateLight)
  register(parallel_array.ScanLight)
  register(parallel_array.ScanMedium)
  register(parallel_array.GroupByLight)
  register(parallel_array.MatrixMultiplication)
  
  // parallel views
  register(parallel_view.DummyViewBenchList.Reduce)
  register(parallel_view.DummyViewBenchList.MediumReduce)
  register(parallel_view.DummyViewBenchList.ModifyThenReduce)
  register(parallel_view.DummyViewBenchList.ModifyThenForce)
  register(parallel_view.DummyViewBenchList.Iteration)
  register(parallel_view.DummyViewBenchList.IterationS)
  register(parallel_view.DummyViewBenchList.IterationM)
  register(parallel_view.DummyViewBenchList.IterationA)
  register(parallel_view.DummyViewBenchList.IterationZ)
  register(parallel_view.DummyViewBenchList.IterationP)
  
  // parallel ranges
  register(parallel_range.RangeBenches.Reduce)
  register(parallel_range.RangeBenches.ReduceMedium)
  register(parallel_range.RangeBenches.ForeachAdd)
  register(parallel_range.RangeBenches.ForeachAddCollatz)
  register(parallel_range.RangeBenches.ForeachModify)
  register(parallel_range.RangeBenches.ForeachModifyMedium)
  register(parallel_range.RangeBenches.ForeachModifyHeavy)
  register(parallel_range.RangeBenches.MapLight)
  register(parallel_range.RangeBenches.MapMedium)
  
  // array benchmarks
  register(arrays.ObjectAccess)
  register(arrays.IntAccess)
  
  // hash benchmarks
  register(hashtries.Foreach)
  register(hashtries.Iterate)
  register(hashtries.Construct)
  register(hashtries.Lookup)
  register(hashtries.Combine)
  register(hashtries.MultipleCombine)
  
  // parallel hash trie benchmarks
  register(hashtries.RefParHashTrieBenches.Reduce)
  register(hashtries.RefParHashTrieBenches.ReduceMedium)
  register(hashtries.RefParHashTrieBenches.Reduce2)
  register(hashtries.RefParHashTrieBenches.Map)
  register(hashtries.RefParHashTrieBenches.Map2)
  
  // parallel hash table map benchmarks
  register(hashtables.RefParHashTableBenches.Reduce)
  register(hashtables.RefParHashTableBenches.Reduce2)
  register(hashtables.RefParHashTableBenches.Foreach)
  register(hashtables.RefParHashTableBenches.ForeachSet)
  register(hashtables.RefParHashTableBenches.Map)
  register(hashtables.RefParHashTableBenches.Map2)
  register(hashtables.RefParHashTableBenches.HeavyMap)
  register(hashtables.RefParHashTableBenches.Filter)
  register(hashtables.RefParHashTableBenches.FlatMap)
  register(hashtables.RefParHashTableBenches.FlatMap2)

  // parallel hash table set benchmarks
  register(hashtables.RefParHashTableSetBenches.Reduce)
  register(hashtables.RefParHashTableSetBenches.Reduce2)
  register(hashtables.RefParHashTableSetBenches.Foreach)
  register(hashtables.RefParHashTableSetBenches.ForeachSet)
  register(hashtables.RefParHashTableSetBenches.Map)
  register(hashtables.RefParHashTableSetBenches.Map2)
  register(hashtables.RefParHashTableSetBenches.HeavyMap)
  register(hashtables.RefParHashTableSetBenches.Filter)
  register(hashtables.RefParHashTableSetBenches.FlatMap)
  
  // general examples
  register(misc.Coder)
  register(misc.Loader)
}


/**
 * Serves as an entrypoint to run all the benchmarks.
 */
object Benchmarking extends BenchmarkRegister {
  
  def printHelp {
    println("Must enter at least four arguments: <collection> <benchmark> <size of the collection> <type>")
    println("  Example: ParArray reduce-light 50000 par")
    println("  Example: ParArray -all 50000 par")
    println
    println("General synthax: <collection> <benchmark> <size> <type> <parallelism-level>")
    println("          <collection>  - name of the collection to test, `-all` runs benchmarks for all collections")
    println("           <benchmark>  - name of the specific benchmark, `-all` runs all benchmarks for the chosen collections")
    println("                <size>  - the size (number of elements) of the collection, or `-default` for default size per benchmark")
    println("                <type>  - `seq` for benchmarking sequential version of the functionality")
    println("                          `par` for benchmarking parallel version of the functionality")
    println("                          `<something-else>` for running comparison benchmarks")
    println("                          `-all` for running sequential, parallel and comparison benchmarks")
    println("   <parallelism-level>  - the level of parallelism used (default 2)")
  }
  
  def otherOptions(args: Array[String]) {
    if (args.length == 0) printHelp
    else args(0) match {
      case "-list" => // lists all benchmarks
        for (bc <- benchcreators) println(bc.fullname)
      case _ => printHelp
    }
  }
  
  def main(args: Array[String]) {
    if (args.length < 4) {
      otherOptions(args)
      return
    }
    
    val collname = args(0)
    val benchname = args(1)
    val size = if (args(2) == "-default") -1 else args(2).toInt
    val tpe = args(3)
    val parlevel = if (args.length >= 5) args(4).toInt else 2
    
    // find all benchmarks to run
    val benches = benchcreators.filter(comp => {
      (collname, benchname) match {
        case ("-all", "-all") => true
        case ("-all", bn) if (benchname != "-all") => bn == comp.benchName
        case (cn, "-all") if (collname != "-all") => cn == comp.collectionName
        case (cn, bn) => cn == comp.collectionName && bn == comp.benchName
      }
    }).flatMap(comp => {
      val collsz = if (size != -1) size else comp.defaultSize
      if (tpe != "-all") List(comp.apply(collsz, parlevel, tpe))
      else for (benchtype <- "seq" :: "par" :: comp.comparisons) 
        yield comp.apply(collsz, parlevel, benchtype)
    })
    
    println("Running benchmarks...")
    for (b <- benches) b.executeBenchmark
  }
  
}



















