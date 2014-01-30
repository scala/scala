import scala.reflect.macros.whitebox.Context
import language.experimental.macros

case class C[T](t: String)
object C {
  implicit def foo[T]: C[T] = macro Macros.impl[T]
}

object Macros {
  def impl[T](c: Context)(ttag: c.WeakTypeTag[T]) = {
    import c.universe._
    import internal._
    val ttag0 = ttag;
    {
      // When we're expanding implicitly[C[Nothing]], the type inferencer will see
      // that foo[T] returns C[T] and that we request an implicit of type C[Nothing].
      //
      // Then the type inferencer will try to match C[T] against C[Nothing] and infer everything it can infer
      // from that match, but not more (e.g. if we were returning Iso[T, U] and the type we were looking at was Iso[Foo, L],
      // we wouldn't want U to be auto-inferred to Nothing, as it usually happens with normal methods,
      // but would rather want it to remain unknown, so that our macro could take a stab at inferring it:
      // see the comments in this commit for more information).
      //
      // Equipped with common sense, in our case of C[T] and C[Nothing] we would expect T to be inferred as Nothing, and then we
      // would expect T in the corresponding macro invocation to be Nothing. Unfortunately it is not that simple.
      //
      // Internally the type inferencer uses Nothing as a dummy value, which stands for "don't know how to
      // infer this type parameter". In the Iso example, matching Iso[T, U] against Iso[Foo, L] would result in
      // T being inferred as Foo and U being inferred as Nothing (!!). Then the type inferencer will think:
      // "Aha! U ended up being Nothing. This means that I failed to infer it,
      // therefore the result of my work is: T -> Foo, U -> still unknown".
      //
      // That's all very good and works very well until Nothing is a genuine result of type inference,
      // as in our original example of inferring T in C[T] from C[Nothing]. In that case, the inferencer becomes confused
      // and here in the macro implementation we get weakTypeOf[T] equal to some dummy type carrying a type parameter
      // instead of Nothing.
      //
      // This eccentric behavior of the type inferencer is a long-standing problem in scalac,
      // so the best one can do for now until it's fixed is to work around, manually converting
      // suspicious T's into Nothings. Of course, this means that we would have to approximate,
      // because there's no way to know whether having T here stands for a failed attempt to infer Nothing
      // or for a failed attempt to infer anything, but at least we're in full control of making the best
      // of this sad situation.
      implicit def ttag: WeakTypeTag[T] = {
        val tpe = ttag0.tpe
        val sym = tpe.typeSymbol.asType
        if (sym.isParameter && !isSkolem(sym)) TypeTag.Nothing.asInstanceOf[TypeTag[T]]
        else ttag0
      }
      reify(C[T](c.Expr[String](Literal(Constant(weakTypeOf[T].toString))).splice))
    }
  }
}