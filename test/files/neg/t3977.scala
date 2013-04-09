trait Bool {
  type If[T]
}

trait False extends Bool {
  type If[F] = F
}

class Field[E, N <: Bool](implicit val w: N#If[E]) {
  type NoNull = Field[E, False]

  new NoNull
}
