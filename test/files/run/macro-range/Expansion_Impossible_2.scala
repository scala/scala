import reflect.macros.Context

object Impls {
  def foreach(c: Context)(f: c.Expr[Int => Unit]): c.Expr[Unit] = {
    // todo. read the compiler config and print if -Ydebug is set
    //println("macro-expand, _this = "+ _this)
    object utils extends Utils { val context: c.type = c }
    import utils._
    import c.universe._
    import Flag._

    val initName = nme.CONSTRUCTOR
    // Either:
    //   scala"{ var i = $low; val h = $hi; while (i < h) { $f(i); i = i + 1 } }
    // or:
    //   scala"($_this: RangeDefault).foreach($f)"
    c.Expr(c.prefix.tree match {
      case Apply(Select(New(tpt), initName), List(lo, hi)) if tpt.symbol.fullName == "Range" =>
        val iname = newTermName("$i")
        val hname = newTermName("$h")
        def iref = Ident(iname)
        def href = Ident(hname)
        val labelname = newTermName("$while")
        val cond = makeBinop(iref, "$less", href)
        val body = Block(
            List(makeApply(f.tree, List(iref))),
            Assign(iref, makeBinop(iref, "$plus", Literal(Constant(1)))))
        val generated =
        Block(
          List(
            ValDef(Modifiers(MUTABLE), iname, TypeTree(), lo),
            ValDef(Modifiers(), hname, TypeTree(), hi)),
          makeWhile(labelname, cond, body))
        // todo. read the compiler config and print if -Ydebug is set
        //tools.nsc.util.trace("generated: ")(generated)
        generated
      case _ =>
        Apply(
          Select(
            Typed(c.prefix.tree, Ident(newTypeName("RangeDefault"))),
            newTermName("foreach")),
          List(f.tree))
    })
  }
}

class Range(val from: Int, val to: Int) extends RangeDefault {
  override def foreach(f: Int => Unit): Unit = macro Impls.foreach
}

object Test extends App {
  new Range(1, 10) foreach println
}