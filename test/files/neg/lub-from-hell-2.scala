class Test {
  trait Tree
  def foo(a: Boolean, b: collection.mutable.Set[Any], c: collection.mutable.ListBuffer[Any]) = if (a) b else c
  def bar(a: Boolean, b: scala.collection.mutable.SetLike[Any,scala.collection.mutable.Set[Any]], c: scala.collection.mutable.Buffer[Any]) = if (a) b else c
  // bar produces an ill-bounded LUB in 2.11.8. After this commit, which fixes a bug in existential+refinement lubs, foo also fails.
}
