object Hang {
  Map[(Int,Int),List[Int]]() ++ (for(x <- 0 to 1 ; y <- 0 to 1) yield {(x,y)-> (0 to 1)})
}