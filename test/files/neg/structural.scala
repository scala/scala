object Test extends Application {

  def f(x: { type D; def m: D }) = x.m

  class Tata

  abstract class Toto[A <: AnyRef] {
    type B <: AnyRef

    def f1[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: A): AnyRef; val x: A }) = x.m[Tata](x.x) //fail
    def f2[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: B): AnyRef; val x: B }) = x.m[Tata](x.x) //fail
    def f3[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: C): AnyRef; val x: C }) = x.m[Tata](x.x) //fail
    def f4[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: D): AnyRef; val x: D }) = x.m[Tata](x.x) //suceed
    def f5[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: E): AnyRef; val x: Tata }) = x.m[Tata](x.x) //suceed

    def f6[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: AnyRef): A }) = x.m[Tata](new AnyRef) //suceed
    def f7[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: AnyRef): B }) = x.m[Tata](new AnyRef) //suceed
    def f8[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: AnyRef): C }) = x.m[Tata](new AnyRef) //suceed
    def f9[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: AnyRef): D }) = x.m[Tata](new AnyRef) //suceed
    def f0[C <: AnyRef](x: AnyRef{ type D <: AnyRef; def m[E >: Null <: AnyRef](x: AnyRef): E }) = x.m[Tata](new AnyRef) //suceed

  }

  val tata = new Tata
  val toto = new Toto[Tata] {
    type B = Tata
  }

  toto.f1[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: Tata): AnyRef = null; val x = tata })
  toto.f2[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: Tata): AnyRef = null; val x = tata })
  toto.f3[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: Tata): AnyRef = null; val x = tata })
  toto.f4[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: D): AnyRef = null; val x = tata })
  toto.f5[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: E): AnyRef = null; val x = tata })

  toto.f6[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: AnyRef): Tata = null })
  toto.f7[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: AnyRef): Tata = null })
  toto.f8[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: AnyRef): Tata = null })
  toto.f9[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: AnyRef): D = null })
  toto.f0[Tata](new AnyRef{ type D = Tata; def m[E >: Null <: AnyRef](x: AnyRef): E = null })

  /* Bug #1246 */
  type Summable[T] = { def +(v : T) : T }
  def sum[T <: Summable[T]](xs : List[T]) = xs.reduceLeft[T](_ + _)

}