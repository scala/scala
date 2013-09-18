trait HOSeq {
  trait Accumulator[+coll[x], elT]
  trait Iterable[+t] {
    type m[+x]
    def accumulator[t]: Accumulator[m, t]
  }
  implicit def listAccumulator[elT]: Accumulator[List, elT] = new Accumulator[List, elT] {}
  trait List[+t] extends Iterable[t] {
    type m[+x] = List[x]
    def accumulator[t]: Accumulator[List, t] = listAccumulator[t]
  }
}