trait Foo {
  def flatMap[RT <: RBound[RT], RBound[_], Result[x <: RBound[x]]]: Result[RT]
// bounds for RT& = >: scala.this.Nothing <: RBound&[RT&]
                                   // bounds for x = >: scala.this.Nothing <: RBound&[x]
}
