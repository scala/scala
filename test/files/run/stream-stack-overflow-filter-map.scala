object Test extends App {
  val resFMap1 = (1 to 10000).toStream filter (_ => false) flatMap (Seq(_))
  val resMap1 = (1 to 10000).toStream filter (_ => false) map (_ + 1)
  assert(resMap1.isEmpty)
  assert(resFMap1.isEmpty)

  //This risks causing a stack overflow 
  val resFMap2 = (1 to 10000).toStream withFilter (_ => false) flatMap (Seq(_))
  val resMap2 = (1 to 10000).toStream withFilter (_ => false) map (_ + 1)
  assert(resMap1 == resMap2)
  assert(resFMap1 == resFMap2)
}
