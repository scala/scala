object Main {
  def f[a]: List[List[a]] = for (val l1 <- Nil; val l2 <- Nil) yield l1
}
