object Test {
  def a2p[a,b,c](f:((a,b))=>c,v:(a,b)):c = f(v)
  a2p(x => x._1,(2,3))
}
