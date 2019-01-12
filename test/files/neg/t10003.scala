trait Functor[a] { type MyType[a] }
object Functor {
  def listFunctor[a]: Functor[a] { type MyType[x] = List[x] } =
    new Functor[a] { type List[t] = List[t] }
}
