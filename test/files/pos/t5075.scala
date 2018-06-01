// Derived from Scalaz - http://scalaz.googlecode.com/svn/continuous/latest/browse.sxr/scalaz/PartialApplys.scala.html

trait PartialApply1Of2[T[_, _], A] {  type Apply[B] = T[A, B] }

trait HKT[D[_]]
trait HKTBounded[C[X] <: Traversable[X], D[_]]
trait Cov[+T]

class Test {
    // exercise type constructor inference in different ways
    implicit def m[D[_]](t: HKT[D]): Int = 1
    def mCov[D[_]](t: Cov[HKT[D]]): Any = ???
    def mBounded[C[X] <: Traversable[X], D[_]](t: Cov[HKTBounded[C, D]]): Any = ???

    val param: HKT[PartialApply1Of2[Tuple2, Int]#Apply] = ???
    m[PartialApply1Of2[Tuple2, Int]#Apply](param): Int // Already compiled
    m(param) // Compiles now
    param: Int // Compiles now

    val paramCov: Cov[HKT[PartialApply1Of2[Tuple2, Int]#Apply]] = ???
    mCov[PartialApply1Of2[Tuple2, Int]#Apply](paramCov)
    mCov(paramCov)

    val paramBounded: Cov[HKTBounded[Traversable, PartialApply1Of2[Tuple2, Int]#Apply]] = ???
    mBounded[Traversable, PartialApply1Of2[Tuple2, Int]#Apply](paramBounded)
    mBounded(paramBounded)
}
