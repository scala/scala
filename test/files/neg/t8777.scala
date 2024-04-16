//> using options '-Wconf:msg=shadowing a nested class of a parent is deprecated:s'

package a {
  trait Test {
    class Shadow
    def test: Shadow = new Shadow
  }
}
package b {
  trait Test extends a.Test {
    class Shadow extends super.Shadow
    override def test: Shadow = super.test
  }
}
