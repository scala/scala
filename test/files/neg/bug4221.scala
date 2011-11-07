class Cl {
        class Sub[TheSub <: Sub[TheSub]] 
}

case class Wrapper[T](v: T)

object O {
  def wrap[S <: Cl#Sub[S]](v: S): Wrapper[S] = {
  }
}
