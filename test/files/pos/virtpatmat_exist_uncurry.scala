object Test {
  trait Leaf[T] {
    def collect[U](f: PartialFunction[Leaf[_], U]): List[U]
    def leaves: List[Leaf[T]] = collect { case l: Leaf[T] => l }
  }
}