package test

trait Iterable[+A] { self =>

  type CC[B] <: Iterable[B] { type CC[C] = self.CC[C] }

}
