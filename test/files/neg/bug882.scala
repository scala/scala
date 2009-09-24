package test;
trait SortedSet[A <% Ordered[A]] {
  def first : A;
  def last : A;
  assert(first.compare(last) < 0);
}
