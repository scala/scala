object ORSet {
  def empty[A]: ORSet[A] = ???
}

final class ORSet[A] {
  def add(node: Long, element: A): ORSet[A] = ???
  def add(node: Int, element: A): ORSet[A] = ???
}

class Test {
  ORSet.empty.add(42, "A")
}
