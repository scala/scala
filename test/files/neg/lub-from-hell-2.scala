// f-bounded types involved in LUBs can sometimes produce an ill-bounded LUB.
class Test {
  trait Tree
  def foo(a: Boolean, b: List[Any], c: collection.mutable.ListBuffer[Any]) = if (a) b else c
}
