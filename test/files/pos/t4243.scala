



object wrap {
  
  trait DomainLike[@specialized(Int) A, +This <: Domain[A]]
  
  trait Domain[@specialized(Int) B]
  extends DomainLike[B, Domain[B]]
  
  trait IterableDomainLike[@specialized(Int) C, +This <: IterableDomain[C]]
  extends DomainLike[C, This]
  
  trait IterableDomain[@specialized(Int) D]
  extends Domain[D] with IterableDomainLike[D, IterableDomain[D]]
  
}
