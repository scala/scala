
// desugars to package p { object `package` }
// previously, synthetic p.C was incorrectly added to this tree
// This only matters because synthetics are not hygienic
package object p

package p {
  import scala.concurrent.Future
  case class C protected[p] (value: Future[Int])   // protected to avoid rewriting C.apply to new C
}

package client {
  trait X {
    import scala.concurrent.Future
    def f = p.C(Future(42)(null))  // ensure synthetics were generated, i.e., p.C.apply
  }
}
