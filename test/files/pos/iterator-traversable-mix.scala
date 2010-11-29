object Test {
  for {
    x1 <- List(1, 2)
    x2 <- Iterator(3, 4)
    x3 <- Seq(5, 6).iterator
    x4 <- Stream(7, 8)
  } yield x1+x2+x3+x4
}
