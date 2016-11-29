trait Test1 {
  abstract class Setting
  def Bool: Setting

  class C[T <: Setting](val s: T)
  val setting1 = null.asInstanceOf[_1.s.type forSome { val _1: C[Setting] }]
  // the derived accessor for this val was not using an inferred type, as was
  // the intention of the implementation in MethodSynthesis.
}
