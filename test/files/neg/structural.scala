object Test extends App {

  def f(x: { type D; def m: D }): Null = null

  class Tata

  abstract class Toto[A <: Object] {
    type B <: Object

    def f1[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: A): Object; val x: A }) = x.m[Tata](x.x) //fail
    def f2[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: B): Object; val x: B }) = x.m[Tata](x.x) //fail
    def f3[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: C): Object; val x: C }) = x.m[Tata](x.x) //fail
    def f4[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: D): Object; val x: D }) = x.m[Tata](x.x) //fail
    def f5[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: E): Object; val x: Tata }) = x.m[Tata](x.x) //succeeds

    def f6[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: Object): A }) = x.m[Tata](null) //succeeds
    def f7[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: Object): B }) = x.m[Tata](null) //succeeds
    def f8[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: Object): C }) = x.m[Tata](null) //succeeds
    def f9[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: Object): D }) = x.m[Tata](null) //fail
    def f0[C <: Object](x: Object{ type D <: Object; def m[E >: Null <: Object](x: Object): E }) = x.m[Tata](null) //succeeds

  }

  val tata = new Tata
  val toto = new Toto[Tata] {
    type B = Tata
  }

  //toto.f1[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Tata): Object = null; val x = tata })
  //toto.f2[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Tata): Object = null; val x = tata })
  //toto.f3[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Tata): Object = null; val x = tata })
  //toto.f4[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: D): Object = null; val x = tata })
  toto.f5[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: E): Object = null; val x: Test.Tata = tata })

  toto.f6[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Object): Tata = null })
  toto.f7[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Object): Tata = null })
  toto.f8[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Object): Tata = null })
  //toto.f9[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Object): D = null })
  toto.f0[Tata](new Object{ type D = Tata; def m[E >: Null <: Object](x: Object): E = null })

  /* Bug #1246 */
  type Summable[T] = { def +(v : T) : T }
  def sum[T <: Summable[T]](xs : List[T]) = xs.reduceLeft[T](_ + _)

  /* Bug #1004 & #967 */
  type S1 = { def f(p: this.type): Unit }
  val s1 = new { def f(p: this.type): Unit = () }

  type S2 = { type T; def f(p: T): Unit }
  //val s2: S2 = new { type T = A; def f(p: T): Unit = () }

  def s3[U >: Null <: Object](p: { def f(p: U): Unit; def u: U }) = ()

}
