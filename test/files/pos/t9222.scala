object Test {
  trait Elem {
    type Peer
  }

  trait Impl[E[x] <: Elem { type Peer = x }] {
    def foo[R](peer: E[R]#Peer) = ()
    foo[Int](??? : E[Int]#Peer)
  }
}
