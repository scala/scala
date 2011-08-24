object Main {
  def f[a]: List[List[a]] = for (l1 <- Nil; l2 <- Nil) yield l1
}
