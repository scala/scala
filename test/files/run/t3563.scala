




// ticket #3563
object Test {
  
  def main(args: Array[String]) {
    var sum = 0
    val setseq = Set(1, 2, 3, 4).toSeq
    setseq.map( n => { sum += n; n * n }).head
    assert(sum == 10)
    
    sum = 0
    val mapseq = Map(1 -> 1, 2 -> 2, 3 -> 3, 4 -> 4).toSeq
    mapseq.map( n => { sum += n._1; (n._1 + n._1, n._2 * n._2) }).head
    assert(sum == 10)
  }
  
}
