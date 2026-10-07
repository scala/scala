class L(val x: Int) extends AnyVal
class M(val s: String) extends AnyVal
class N[T](val t: T) extends AnyVal
case class O(s: String) extends AnyVal

object Test {
  def main(args: Array[String]): Unit = {
    val l = new L(0)
    assert(l.hashCode == 0)
    assert(l.toString == "L@0")

    val m = new M(null)
    assert(m.hashCode == 0)
    assert(m.toString == "M@0")
    assert(m == new M(null))
    val mm = new M("")
    assert(mm.toString == "M@0")
    assert(mm.hashCode == 0)
    assert(m != mm)

    val n = new N(new N(new M(null)))
    assert(n.hashCode == 0)
    assert(n.toString == "N@0")
    assert(n != new N(new M(null)))

    val o = O(null)
    assert(o.hashCode == 0)
    assert(o.toString == "O(null)")
    assert(o == O(null))
    val oo = O("")
    assert(oo.hashCode == 0)
    assert(oo.toString == "O()")
    assert(o != oo)

    val map = collection.mutable.HashMap.empty[M, Int]
    map(m) = 1
    map(mm) = 2
    assert(map(m) == 1)
    assert(map(new M(null)) == 1)
    assert(map(new M("")) == 2)
  }
}
