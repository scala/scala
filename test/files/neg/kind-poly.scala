object Test {

  ////////////////////////////////////////////////
  // PoC of controlled KindPolymorphism in Scala
  //
  // The idea is NOT to provide universal kind-polymorphism that would be a bad idea anyway
  // but to bring a "controlled" kind-polymorphism relying on accepted kinds defined by typeclass implicits
  // Thus, kind-polymorphism is strictly scoped to your domain and is what you expect to be, nothing else.
  //
  // `Ykind-polymorphism` flag aims at deferring just a bit Scalac type inference when encountering AnyKind higher bounds
  // without losing any strictness in the final typing.
  // `<: AnyKind` type-bound is purely technicaland totally eliminated after erasure. There is not type associated to it.
  // 
  // Here are code-samples that work now:
  //    - basic kind polymorphism controlled by implicits
  //    - Kindness proofs based on typeclasses (specially SameKind)
  //    - Kind-Polymorphic list (on type & value) (2 different implementations)
  //    - Some weird cases we don't want the compiler to authorize

  def foo0[F <: AnyKind]: F = null.asInstanceOf[F]
  foo0[List]                // KO -> neg
  val l = foo0[List]        // KO -> neg

  def foo1[F <: AnyKind, A <: AnyKind]: F[A] = ??? // KO

  def foo3: AnyKind = ??? // KO

}
