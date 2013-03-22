class ClassWithExistential {
  def foo[A, B] : A=> B forSome {type A <: Seq[Int]; type B >: String} = null
}
