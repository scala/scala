object Test {
  trait Bounds {
    type Upper <: Bounds
  }

  trait Narrow extends Bounds {
    type Upper >: Narrow <: Bounds
  }

  trait Template[+X <: Bounds] extends Bounds {
    val body :X
    type Bound >: body.Upper <: Bounds
    type Copy[+A <: Bound] <: Template[A]
    type High[T[+A <: Narrow] <: Bounds]

    def applied(narrow: Template[Narrow]): High[narrow.Copy] //ok
    def indirect(narrow: Template[Narrow]): High[({ type T[+A <: Narrow] = narrow.Copy[A] })#T] //also ok
  }
}
