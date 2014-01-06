


import scala.collection._



/** A sorted set should not replace elements when adding
 *  and the element already exists in the set.
 */
object Test {

  class Foo(val name: String, val n: Int) {
    override def equals(obj: Any): Boolean = obj match { case other: Foo => name == other.name; case _ => false }
    override def hashCode = name.##
    override def toString = "Foo(" + name + ", " + n + ")"
  }

  implicit val ordering: Ordering[Foo] = Ordering.fromLessThan[Foo] { (a, b) => a.name.compareTo(b.name) < 0 }

  def check[S <: Set[Foo]](set: S) {
    def output(s: Set[Foo]) = println(s.toList.sorted.mkString(","))
    output(set + new Foo("bar", 2))
    output(set ++ List(new Foo("bar", 2), new Foo("bar", 3), new Foo("bar", 4)))
    output(set union Set(new Foo("bar", 2), new Foo("baz", 3), new Foo("bazz", 4)))
  }

  def main(args: Array[String]) {
    check(Set(new Foo("bar", 1)))
    check(immutable.Set(new Foo("bar", 1)))
    check(mutable.Set(new Foo("bar", 1)))
    check(immutable.SortedSet(new Foo("bar", 1)))
    check(mutable.SortedSet(new Foo("bar", 1)))
  }

}
