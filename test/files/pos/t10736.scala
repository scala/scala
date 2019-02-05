object ImplicitRegression {

  import scala.language.{implicitConversions, higherKinds}

  trait Map[K, +V] extends MapOps[K, V, Map, Map[K, V]]

  trait MapOps[K, +V, +CC[_, _], +C]

  trait BuildFrom[-From, -A, +C]

  object BuildFrom {
    implicit def buildFromMapOps[CC[X, Y] <: Map[X, Y] with MapOps[X, Y, CC, _], K0, V0, K, V]: BuildFrom[CC[K0, V0], (K, V), CC[K, V]] = ???
  }

  trait MapDecorator[K, V] {
    val foo: Map[K, V]
    def zipByKeyWith[W, X, C1](that: Map[K, W])(f: (V, W) => X)(implicit bf: BuildFrom[foo.type, (K, X), C1]): C1 = ???
  }

  implicit def MapDecorator[K, V](map: Map[K, V]): MapDecorator[K, V] { val foo: map.type } = ???

  def test[A, B, C0](map1: Map[A, B], map2: Map[A, B], f: (B, B) => C0): Unit = {
    val map3 = map1.zipByKeyWith(map2)(f)/*(BuildFrom.buildFromMapOps)*/
    map3: Map[A, C0]
  }
}
