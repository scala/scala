trait SeqFactory[CC[X] <: Seq[X]]

class A {
  def f(x: Boolean) = if (x) (null: SeqFactory[List]) else (null: SeqFactory[Set])
}
