class Test {
  trait Tree
  def foo(a: Boolean, b: collection.mutable.Set[Any], c: collection.mutable.ListBuffer[Any]) = if (a) b else c
  def bar(a: Boolean, b: scala.collection.mutable.SetLike[Any,scala.collection.mutable.Set[Any]], c: scala.collection.mutable.Buffer[Any]) = if (a) b else c
  // bar produces an ill-bounded LUB in 2.11.8. After this commit, which fixes a bug in existential+refinement lubs, foo also fails.
}
// This test case minimizes a case that stated to fail compile after my fixes in SI-5294.
// `foo` used to compile for the wrong reason, `mergePrefixAndArgs` failed to transpose a
// ragged matrix and skipped to the next level of the base type sequences to find a common type symbol.
//
// My changes fixed the root cause of the ragged matrix, which uncovered the latent bug.
// For comparison, `bar` failed to compile before _and_ after my changes for the same reason:
// f-bounded types involved in LUBs can sometimes produce an ill-bounded LUB.
