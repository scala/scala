object Test extends App {
  def getClause[T](clauses: List[T]): Option[T] = {
    for (c <- clauses) {
      return Some(c)
    }
    return None
  }
  println(getClause(List(1, 2, 3)))
}
