object Test extends App {
  List[List[Any]]().transpose.isEmpty
  Array[Array[Any]]().transpose.isEmpty
  Vector[Vector[Any]]().transpose.isEmpty
  LazyList[LazyList[Any]]().transpose.isEmpty
}
