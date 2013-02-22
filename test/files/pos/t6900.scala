trait Test {
  class Tree
  @annotation.tailrec
  private def check[T <: Tree](tree: T): T = {
    val x: tree.type = tree
    check(x)
  }
}
