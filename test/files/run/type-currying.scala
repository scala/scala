

import scala.language.{ higherKinds, reflectiveCalls }
import scala.collection.{ mutable, immutable, generic }
import generic.CanBuildFrom

object Partial {
  type KnownContainer[CC[K, V] <: collection.Map[K, V]] = {
    def values[V] : KnownValues[CC, V]
    def apply[K] : KnownKeys[CC, K]
  }
  type KnownKeys[CC[K, V] <: collection.Map[K, V], K] = {
    def apply[V](implicit cbf: CanBuildFrom[_, (K, V), CC[K, V]]): CC[K, V]
  }
  type KnownValues[CC[K, V] <: collection.Map[K, V], V] = {
    def apply[K](implicit cbf: CanBuildFrom[_, (K, V), CC[K, V]]): CC[K, V]
  }

  def apply[CC[K, V] <: collection.Map[K, V]] : KnownContainer[CC] = new {
    def values[V] : KnownValues[CC, V] = new {
      def apply[K](implicit cbf: CanBuildFrom[_, (K, V), CC[K, V]]) = cbf().result
    }
    def apply[K] = new {
      def apply[V](implicit cbf: CanBuildFrom[_, (K, V), CC[K, V]]) = cbf().result
    }
  }
}

object Test {
  val m = Partial[immutable.TreeMap]
  val m1 = m[String]
  val m2 = m[Int][Int]

  val mutableBippy = Partial[mutable.HashMap][String][Int]
  mutableBippy("abc") = 55

  val immutableBippy = Partial[immutable.HashMap].values[Int]
  def make[T](xs: T*) = immutableBippy[T] ++ xs.zipWithIndex

  val n0 = Partial[immutable.HashMap][String][Int] ++ Seq(("a", 1))
  val n1 = Partial.apply[immutable.HashMap].apply[String].apply[Int] ++ Seq(("a", 1))

  def main(args: Array[String]): Unit = {
    println(mutableBippy)
    make('a' to 'z': _*).toList.sorted foreach println
    assert(n0 == n1)
  }
}

class A {
  object Foo {
    def apply[T] = Bar
  }
  object Bar {
    def apply() = Foo
  }

  def f() = Foo
  def g = f()[Int]()[String]()
  def h = Foo[Foo.type]()[Foo.type]()
}
