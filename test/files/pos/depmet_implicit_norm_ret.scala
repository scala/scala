object Test{
  def ?[S <: AnyRef](implicit w : S) : w.type = w
  
  // fallback, lower priority (overloading rules apply: pick alternative in subclass lowest in subtyping lattice)
  class ZipWithDefault {
    implicit def ZeroZipWith[S] = new ZipWith[S] {
      type T = Stream[S]
    }    
  }
  
  object ZipWith extends ZipWithDefault {
    // def apply[S: ZipWith](s : S) = ?[ZipWith[S]].zipWith(s) // TODO: bug return type should be inferred
    def apply[S](s : S)(implicit zw: ZipWith[S]): zw.T = zw.zipWith(s)

    implicit def SuccZipWith[S,R](implicit zWith : ZipWith[R]) = new ZipWith[S => R] {
      type T = Stream[S] => zWith.T // dependent types replace the associated types functionality
    }    
  }
  
  trait ZipWith[S] {
    type T
    def zipWith : S => T = error("")
  }
  
  // bug: inferred return type = (Stream[A]) => java.lang.Object with Test.ZipWith[B]{type T = Stream[B]}#T
  // this seems incompatible with vvvvvvvvvvvvvvvvvvvvvv   -- #3731
  def map[A,B](f : A => B)   /* : Stream[A] => Stream[B]*/ = ZipWith(f) 
  val tst: Stream[Int] = map{x: String => x.length}(Stream("a"))  
}  