class A {
  // Any of Array[List[Symbol]], List[Array[Symbol]], or List[List[Symbol]] compile.
  var xs: Array[Array[Symbol]] = _
  var ys: Array[Map[Symbol, Set[Symbol]]] = _

  xs = Array(Array())
  ys = Array(Map(), Map())
}