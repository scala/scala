object Test extends App {
  println(List(1,2,3) match {
    case Nil => List(0)
// since the :: extractor's argument must be a ::, there has to be a cast before its unapply is invoked
    case x :: y :: z :: a :: xs => xs ++ List(x)
    case x :: y :: z :: xs => xs ++ List(x)
    case _ => List(0)
  })
}
