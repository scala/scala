trait C

class Foo[@specialized(Int) T, A] {
  def bar[B >: A <: C]: T = ???
}
