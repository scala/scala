trait Map[K]
class HashMap[K] extends Map[K]
class IdentityBox[+A]

class IdentityHashMap[K](inner: HashMap[IdentityBox[K]]) extends Map[K] {
  def this(initialMap: Map[_ <: K]) = this(new HashMap[IdentityBox[K]]())

  def bla[K](inner: HashMap[IdentityBox[K]]): IdentityHashMap[K] = ???
  def bla[K](initialMap: Map[_ <: K]): IdentityHashMap[K] = bla(new HashMap[IdentityBox[K]]())

  new IdentityHashMap(??? : HashMap[IdentityBox[K]])
  bla(??? :HashMap[IdentityBox[K]])
}

// isApplicable true : (inner: HashMap[IdentityBox[K]])IdentityHashMap[K] to List(HashMap[IdentityBox[K]]) for ? under List()
