

import collection._


// test conversions between collections
object Test {
  
  def main(args: Array[String]) {
    testConversions
  }
  
  def testConversions {
    // seq conversions
    assertSeq(parallel.mutable.ParArray(1, 2, 3))
    assertSeq(parallel.mutable.ParHashMap(1 -> 2, 2 -> 3))
    assertSeq(parallel.mutable.ParHashSet(1, 2, 3))
    assertSeq(parallel.immutable.ParRange(1, 50, 1, false))
    assertSeq(parallel.immutable.ParHashMap(1 -> 2, 2 -> 4))
    assertSeq(parallel.immutable.ParHashSet(1, 2, 3))
    
    // par conversions
    assertPar(Array(1, 2, 3))
    assertPar(mutable.ArrayBuffer(1, 2, 3))
    assertPar(mutable.ArraySeq(1, 2, 3))
    assertPar(mutable.WrappedArray.make[Int](Array(1, 2, 3)))
    assertPar(mutable.HashMap(1 -> 1, 2 -> 2))
    assertPar(mutable.HashSet(1, 2, 3))
    assertPar(immutable.Range(1, 50, 1))
    assertPar(immutable.HashMap(1 -> 1, 2 -> 2))
    assertPar(immutable.HashSet(1, 2, 3))
    
    // par.to* and to*.par tests
    assertToPar(List(1 -> 1, 2 -> 2, 3 -> 3))
    assertToPar(Stream(1 -> 1, 2 -> 2))
    assertToPar(Array(1 -> 1, 2 -> 2))
    assertToPar(mutable.PriorityQueue(1 -> 1, 2 -> 2, 3 -> 3))
    assertToPar(mutable.ArrayBuffer(1 -> 1, 2 -> 2))
    assertToPar(mutable.ArraySeq(1 -> 3))
    assertToPar(mutable.WrappedArray.make[(Int, Int)](Array(1 -> 3)))
    assertToPar(mutable.HashMap(1 -> 3))
    assertToPar(mutable.HashSet(1 -> 3))
    assertToPar(immutable.HashMap(1 -> 3))
    assertToPar(immutable.HashSet(1 -> 3))
    assertToPar(parallel.mutable.ParArray(1 -> 1, 2 -> 2, 3 -> 3))
    assertToPar(parallel.mutable.ParHashMap(1 -> 2))
    assertToPar(parallel.mutable.ParHashSet(1 -> 2))
    assertToPar(parallel.immutable.ParHashMap(1 -> 2))
    assertToPar(parallel.immutable.ParHashSet(1 -> 3))
    
    assertToParWoMap(immutable.Range(1, 10, 2))
    
    // seq and par again conversions)
    assertSeqPar(parallel.mutable.ParArray(1, 2, 3))
  }
  
  def assertSeqPar[T](pc: parallel.ParIterable[T]) = pc.seq.par == pc
  
  def assertSeq[T](pc: parallel.ParIterable[T]) = assert(pc.seq == pc)
  
  def assertPar[T, P <: Parallel](xs: GenIterable[T]) = assert(xs == xs.par)
  
  def assertToPar[K, V](xs: GenTraversable[(K, V)]) {
    xs match {
      case _: Seq[_] =>
        assert(xs.toIterable.par == xs)
        assert(xs.par.toIterable == xs)
      case _ =>
    }
    
    assert(xs.toSeq.par == xs.toSeq)
    assert(xs.par.toSeq == xs.toSeq)
    
    assert(xs.toSet.par == xs.toSet)
    assert(xs.par.toSet == xs.toSet)
    
    assert(xs.toMap.par == xs.toMap)
    assert(xs.par.toMap == xs.toMap)
  }
  
  def assertToParWoMap[T](xs: GenSeq[T]) {
    assert(xs.toIterable.par == xs.toIterable)
    assert(xs.par.toIterable == xs.toIterable)
    
    assert(xs.toSeq.par == xs.toSeq)
    assert(xs.par.toSeq == xs.toSeq)
    
    assert(xs.toSet.par == xs.toSet)
    assert(xs.par.toSet == xs.toSet)
  }
  
}
