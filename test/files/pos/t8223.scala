package p {
  class ViewEnv[AIn] {
    type A = AIn
    class SubView { def has(x: A): Boolean = ??? }
    def get: SubView = new SubView
  }

  trait HasA { type A }
  trait Indexable[R] extends HasA
  class ArrayTC[AIn] extends Indexable[Array[AIn]] { type A = AIn }
}

package object p {
  implicit def arrayTypeClass[A] : ArrayTC[A] = new ArrayTC[A]
  object intArrayTC extends ArrayTC[Int]

  type EnvAlias[W <: HasA] = ViewEnv[W#A]
  type SubAlias[W <: HasA] = ViewEnv[W#A]#SubView

  def f0[R](xs: R)(implicit tc: Indexable[R]): ViewEnv[tc.A]#SubView     = new ViewEnv[tc.A]() get
  def f1[R](xs: R)(implicit tc: Indexable[R]): EnvAlias[tc.type]#SubView = new ViewEnv[tc.A]() get
  def f2[R](xs: R)(implicit tc: Indexable[R]): SubAlias[tc.type]         = new ViewEnv[tc.A]() get

  def g0 = f0(Array(1)) has 2                   // ok
  def g1 = f1(Array(1)) has 2                   // ok
  def g2 = f2(Array(1)) has 2                   // "found: Int(2), required: tc.A"
  def g3 = f2(Array(1))(new ArrayTC[Int]) has 2 // "found: Int(2), required: tc.A"
  def g4 = f2(Array(1))(intArrayTC) has 2       // ok
}
