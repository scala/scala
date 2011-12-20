import collection.mutable

object Test {
  class IMap0[K[_], V[_]](backing: Map[K[_], V[_]]) {
    def mapSeparate[VL[_], VR[_]](f: V[_] => ({type l[T] = Either[VL[T], VR[T]]})#l[_] ) = {
      backing.view.map { case (k,v) => f(v) match {
	case Left(l) => Left((k, l))
	case Right(r) => Right((k, r))
        }
      }
    }
  }
}
