package tastytest

object OpaqueConstructorParameter {
  opaque type Id[X] <: X = X

  def id[X](x: X): Id[X] = x
}

class OpaqueHolder(val longId: OpaqueConstructorParameter.Id[Long], val stringId: OpaqueConstructorParameter.Id[String])
