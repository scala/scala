trait Monad[T <: Bound[T], MyType[x <: Bound[x]], Bound[_]] {
  def map[S <: Bound[S]](f: T => S): MyType[S] 

  def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],  
              Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
              (f: T => Result[S]): Result[S] 

  def filter(p: T => Boolean): MyType[T]
}

class Set[T <: Ordered[T]] extends Monad[T, Set, Ordered] {
  def map[S <: Ordered[S]](f: T => S): Set[S] = error("TODO") 
  
  def flatMap[S <: RBound[S], RContainer[x <: RBound[x]], RBound[_],  
              Result[x <: RBound[x]] <: Monad[x, RContainer, RBound]]
              (f: T => Result[S]): Result[S]  = error("TODO") 
              
  def filter(p: T => Boolean): Set[T] = error("TODO")               
}
