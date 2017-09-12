package test

import reflect.api.Universe

class Checks[U <: Universe with Singleton](universe: U) {
  import universe._

  def check(subj: ClassSymbol): Unit = {
    val tpe = subj.info

    /* grab the fields */
    val volatile  = tpe.decl(TermName("_volatile"))
    val transient = tpe.decl(TermName("_transient"))
    val synchronized = tpe.decl(TermName("_synchronized"))
    val native = tpe.decl(TermName("_native"))

    /* initialize the infos, sigh */
    volatile.info; transient.info; synchronized.info; native.info

    /* check for the annotations */
    assert(volatile.annotations.exists(_.tree.tpe =:= typeOf[scala.volatile]))
    assert(transient.annotations.exists(_.tree.tpe =:= typeOf[scala.transient]))
    assert(native.annotations.exists(_.tree.tpe =:= typeOf[scala.native]))

    /* and for bonus points...?
     * There appears to be no very good way to check if a method is synchronized
     * in the reflection API. This is probably for the better. If someone wants to
     * come in and add it for the benefit of an unusually intrepid macro author,
     * go right ahead. */
    //import internal._, decorators._
    //assert((synchronized.flags & InternalFlags.SYNCHRONIZED) != 0L)

  }

}

object CheckMacro {
  import language.experimental.macros
  def check[T]: Unit = macro impl[T]

  import reflect.macros.blackbox
  def impl[T: c.WeakTypeTag](c: blackbox.Context): c.Tree = {
    import c.universe._
    new Checks[c.universe.type](c.universe).check(symbolOf[T].asClass)
    Literal(Constant(()))
  }
}