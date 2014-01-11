import scala.reflect.macros.blackbox.Context

object Macros {
  def impl(c: Context) = {
    import c.universe._
    val Expr(Block((cdef: ClassDef) :: Nil, _)) = reify { class C { def x = 2 } }
    val cdef1 =
      new Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case Template(_, _, ctor :: defs) =>
            val defs1 = defs collect {
              case ddef @ DefDef(mods, name, tparams, vparamss, tpt, body) =>
                val future = Select(Select(Ident(TermName("scala")), TermName("concurrent")), TermName("Future"))
                val Future = Select(Select(Ident(TermName("scala")), TermName("concurrent")), TypeName("Future"))
                val tpt1 = if (tpt.isEmpty) tpt else AppliedTypeTree(Future, List(tpt))
                val body1 = Apply(future, List(body))
                val name1 = TermName("async" + name.toString.capitalize)
                DefDef(mods, name1, tparams, vparamss, tpt1, body1)
            }
            Template(Nil, emptyValDef, ctor +: defs ::: defs1)
          case _ =>
            super.transform(tree)
        }
      } transform cdef
    c.Expr[Unit](Block(cdef1 :: Nil, Literal(Constant(()))))
  }

  def foo = macro impl
}
