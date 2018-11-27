object t10228 {
  trait Data { self =>
    type Type >: self.type <: Data
  }
  trait DataF[P <: Data, A <: DataF[P, A]] extends Data { type Type = P#Type }

  type Fili[A <: Data] = DataF[A, A]

  def g[A <: Fili[A]]: Unit = implicitly[A <:< A#Type]
}