package test {

class J {
  def f1(): Int = { return 5; }
  protected def f2(): Int = { return 5; }
}

}

package test {
package nest {

class S1 {
  def g1(x: J) = x.f1()
  def g2(x: J) = x.f2()
}

class S2 extends J {
  def g1(x: J) = x.f1()
  def g2(x: J) = x.f2()
}

}}
