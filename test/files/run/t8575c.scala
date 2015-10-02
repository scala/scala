class C

trait TypeMember {
  type X
  type Y
  type Z
}

object Test extends TypeMember {
  type A = X with Y
  type B = Z with A
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
