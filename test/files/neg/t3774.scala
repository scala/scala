// This used to hang the lub process. Now it rejects the file. This is still not correct,
// but we can solve this only after a redesign of lub a la dot.
object Hang {
  Map[(Int,Int),List[Int]]() ++ (for(x <- 0 to 1 ; y <- 0 to 1) yield {(x,y)-> (0 to 1)})
}
