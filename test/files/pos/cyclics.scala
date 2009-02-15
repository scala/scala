trait Param[T]
trait Abs { type T }
trait Cyclic1[A <: Param[A]]    // works
trait Cyclic2[A <: Abs { type T <: A }]    // fails
trait Cyclic3 { type A <: Abs { type T = A } }    // fails
trait Cyclic4 { type A <: Param[A] }   // works

