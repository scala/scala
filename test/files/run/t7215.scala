object Test extends App {
  List[List[Any]]().transpose.isEmpty
  Array[Array[Any]]().transpose.isEmpty
  Vector[Vector[Any]]().transpose.isEmpty
  Stream[Stream[Any]]().transpose.isEmpty
}
