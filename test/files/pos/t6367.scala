package play.api.libs.json.util

trait FunctionalCanBuild[M[_]]{
  def apply[A,B](ma:M[A], mb:M[B]):M[A ~ B]
}

trait Variant[M[_]]

trait Functor[M[_]] extends Variant[M]{
  def fmap[A,B](m:M[A], f: A => B): M[B]
}

case class ~[A,B](_1:A,_2:B)

class FunctionalBuilder[M[_]](canBuild:FunctionalCanBuild[M]){
  class CanBuild20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](
    m1:M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19],
    m2:M[A20]
  ) {

    def ~[A21](m3:M[A21]) = new CanBuild21(canBuild(m1,m2),m3)

    def apply[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B)(implicit fu:Functor[M]): M[B] =
      fu.fmap[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20, B](
        canBuild(m1, m2),
        { case a1 ~ a2 ~ a3 ~ a4 ~ a5 ~ a6 ~ a7 ~ a8 ~ a9 ~ a10 ~ a11 ~ a12 ~ a13 ~ a14 ~ a15 ~ a16 ~ a17 ~ a18 ~ a19 ~ a20 =>
              f(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20) }
      )
  }

  class CanBuild21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](m1:M[A1 ~ A2 ~ A3 ~ A4 ~ A5 ~ A6 ~ A7 ~ A8 ~ A9 ~ A10 ~ A11 ~ A12 ~ A13 ~ A14 ~ A15 ~ A16 ~ A17 ~ A18 ~ A19 ~ A20], m2:M[A21]){
  }

}
