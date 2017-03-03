class A
class B
class C

object Test {
  type F = A with B

  def main(args: Array[String]) {
    import reflect.runtime.universe._
    val t1 = typeOf[F with C]
    val t2 = typeOf[(A with B) with C]
    val t3 = typeOf[A with B with C]
    assert(t1 =:= t2)
    assert(t2 =:= t3)
    assert(t3 =:= t1)
  }
}
