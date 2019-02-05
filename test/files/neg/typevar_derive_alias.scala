// documenting the status quo
// all toSetMapN should type check, but 2 & 3 do not right now -- all type check in dotty!
// (Before, the non-type-alias version type checked due to some gross hack -- I removed it so now they both fail)
class Test {
  trait St[T] { def map[U](f: T => U): St[U] }
  trait Sq[+T] { def toSt[B >: T]: St[B] }

  trait El
  def toSetMap1(ts: Sq[El]): St[El] = {
    val st = ts.toSt // here we infer B to be El because B occurs invariantly
    st.map(x => x)
  }

  def toSetMap2(ts: Sq[El]): St[El] = ts.toSt.map(x => x) // B occurs contravariantly, so we maximize

  type Alias = El
  def toSetMap3(ts: Sq[Alias]): St[Alias] = ts.toSt.map(x => x)
}
