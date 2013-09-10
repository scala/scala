object Test1 {
  def f[T](xs: Array[T]): Array[T] = xs match { case xs => xs }
  // [check: patmat] The symbol, tpe or info of tree `(x) : Array[T]` refers to a out-of-scope symbol, type T. tree.symbol.ownerChain: value x
  // [check: patmat] The symbol, tpe or info of tree `(x) : Array[T]` refers to a out-of-scope symbol, type T. tree.symbol.ownerChain: value x

  def g[T](xs: Array[T]): Array[T] = {
    val x1: Array[T] = xs
    def case4() = matchEnd3(x1)
    def matchEnd3(x: Array[T]) = x
    case4()
  }
}
