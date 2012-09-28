// This should be allowed
class Ok[T <: Comparable[_ >: T]]

// This is (il)legitimately a cyclic reference
class NotOk[T <: Comparable[_ <: T]]
