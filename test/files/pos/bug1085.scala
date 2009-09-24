trait Functor[a] {
	type MyType[a]
}

object Test {
  def listFunctor[a]: Functor[a]{type MyType[x]=List[x]} = new Functor[a] {
    type MyType[t]=List[t]
  }
}
