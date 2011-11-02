trait A { type T; type M >: T }
trait B extends A { 
  val x : String; 
  val u : A { type T = B.this.T } ; 
  type T = x.type; 
  type M = u.M 
}
