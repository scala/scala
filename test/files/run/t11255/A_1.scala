//> using options -opt:inline:**
class K(val f: Int => Int) extends Serializable
class A {
  @inline final def f = new K(x => x + 1)
}
