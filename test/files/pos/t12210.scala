trait SpecFun[@specialized T] {
  type Res
  def res: Res
}

object Test {
  def m[@specialized T](op: SpecFun[T]): op.Res = op.res
}

trait ValuesVisitor[A] {
  def visit(a: A): Unit
  def visitArray(arr: Array[A]): Unit = ???
}

class OpArray[@specialized A] {
  def traverse(from: Array[A], fn: ValuesVisitor[A]): fn.type = {
    fn.visitArray(from)
    fn
  }
}
