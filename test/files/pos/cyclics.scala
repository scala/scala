trait Param[T]
trait Abs { type T }
trait Cyclic1[A <: Param[A]]    // works
trait Cyclic2[A <: Abs { type T <: A }]   
trait Cyclic3 { type A <: Abs { type T = A } }    
trait Cyclic4 { type A <: Param[A] }   // works
trait Cyclic5 { type AA <: Abs; type A <: AA { type T = A } }    


trait IterableTemplate {
  type Elem
  type Constr <: IterableTemplate
  type ConstrOf[A] = Constr { type Elem = A }
  
  def iterator: Iterator[Elem]
  
  def map [B] (f: Elem => B): ConstrOf[B]
  
  def foreach(f: Elem => Unit) = iterator.foreach(f)
}


trait Iterable[A] extends IterableTemplate { self =>
  type Elem 
  type Constr <: Iterable[A] { type Constr <: Iterable.this.Constr }
}
