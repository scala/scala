class Capture
class Test {
  def test1 = (i: Int) => ""
  def test2 = (i: VC) => i
  def test3 = (i: Int) => i

  def test4 = {val c = new Capture; (i: Int) => {(c, Test.this.toString); 42} }
  def test5 = {val c = new Capture; (i: VC) => (c, Test.this.toString) }
  def test6 = {val c = new Capture; (i: Int) => (c, Test.this.toString) }

  def test7 = {val vc = new Capture; (i: Int) => vc }
  def test8 = {val c = 42; (s: String) => (s, c)}
  def test9 = {val c = 42; (s: String) => ()}
}

object Test {
  def main(args: Array[String]): Unit = {
    val t = new Test
    assert(t.test1.apply(42) == "")
    assert(t.test2.apply(new VC(42)) == new VC(42))
    assert(t.test3.apply(-1) == -1)
    t.test4.apply(0)
    t.test5.apply(new VC(42))
    t.test6.apply(42)
    t.test7.apply(0)
    t.test8.apply("")
    t.test9.apply("")
  }
}
