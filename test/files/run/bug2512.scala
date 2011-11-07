import scala.tools.nsc.util.HashSet

object Test {
  val runs = 10000
  class Bop
  
  def main(args: Array[String]): Unit = {
    val set: HashSet[Bop] = HashSet("Bop", 16)
    (1 to runs).toList foreach (_ => set addEntry new Bop)
    
    assert(runs == set.size && set.size == set.iterator.length)
  }
}
