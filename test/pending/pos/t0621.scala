object Test {
  val x1 : List[T] forSome { type T } = List(42)
  val w1 = x1 match { case y : List[u] => ((z : u) => z)(y.head) }
  
  val x2 : T forSome { type T } = 42
  val w2 = x2 match { case y : u => ((z : u) => z)(y) }
}
