object Test {
  def idMap[C[_],T](m: { def map[U](f: T => U): C[U] }): C[T] = m.map(t => t)
  
  def main(args: Array[String]): Unit = {
    idMap(Some(5))
    idMap(Responder.constant(5))
  }
}
