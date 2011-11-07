


import scala.collection.immutable._


// ticket #3511
object Test {
  
  def main(args: Array[String]) {
    assert(Stream.from(0).view.force.take(5) == List(0, 1, 2, 3, 4))
    
    val s = Stream.from(0)
    val smap = s.view.map(_ * 2).force.take(5)
    assert(smap == List(0, 2, 4, 6, 8))
    
    val sfilter = s.view.filter(_ % 2 == 0).force.take(5)
    assert(sfilter == List(0, 2, 4, 6, 8))
    
    val sflatmap = s.view.flatMap(n => List(n, n * 2)).force.take(6)
    assert(sflatmap == List(0, 0, 1, 2, 2, 4))
    
    val stakewhile = s.view.takeWhile(_ < 10).force
    assert(stakewhile == List.range(0, 10))
    
    val szip = s.view.zip(s.map(_ / 2)).force.take(5)
    assert(szip == List((0, 0), (1, 0), (2, 1), (3, 1), (4, 2)))
    
    val szipall = s.view.zipAll(List(0, 1, 2), 0, 0).force.take(5)
    assert(szipall == List((0, 0), (1, 1), (2, 2), (3, 0), (4, 0)))
    
    val spatch = s.view.patch(1, List(5, 5, 5), 5).force.take(5)
    assert(spatch == List(0, 5, 5, 5, 6))
  }
  
}
