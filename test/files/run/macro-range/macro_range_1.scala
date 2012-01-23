import reflect.api.Modifier
import reflect.macro.Context

abstract class RangeDefault {
  val from, to: Int
  def foreach(f: Int => Unit) = {
    var i = from
    while (i < to) { f(i); i += 1 }
  }
}

/** This class should go into reflect.macro once it is a bit more stable. */
abstract class Utils {
  val context: Context
  import context._

  class TreeSubstituter(from: List[Symbol], to: List[Tree]) extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(_) =>
        def subst(from: List[Symbol], to: List[Tree]): Tree =
          if (from.isEmpty) tree
          else if (tree.symbol == from.head) to.head.duplicate // TODO: does it ever make sense *not* to perform a shallowDuplicate on `to.head`?
          else subst(from.tail, to.tail);
        subst(from, to)
      case _ =>
        val tree1 = super.transform(tree)
        if (tree1 ne tree) tree1.tpe = null
        tree1
    }
  }
  def makeApply(fn: Tree, args: List[Tree]): Tree = fn match {
    case Function(vparams, body) =>
      new TreeSubstituter(vparams map (_.symbol), args) transform body
    case Block(stats, expr) =>
      Block(stats, makeApply(expr, args))
    case _ =>
      // todo. read the compiler config and print if -Ydebug is set
      //println("no beta on "+fn+" "+fn.getClass)
      Apply(fn, args)
  }
  def makeWhile(lname: TermName, cond: Tree, body: Tree): Tree = {
    val continu = Apply(Ident(lname), Nil)
    val rhs = If(cond, Block(List(body), continu), Literal(Constant()))
    LabelDef(lname, Nil, rhs)
  }
  def makeBinop(left: Tree, op: String, right: Tree): Tree =
    Apply(Select(left, newTermName(op)), List(right))
}

class Range(val from: Int, val to: Int) extends RangeDefault {
  override def macro foreach(f: Int => Unit): Unit = {
    // todo. read the compiler config and print if -Ydebug is set
    //println("macro-expand, _this = "+ _this)
    import _context._
    object utils extends Utils {
      val context: _context.type = _context
    }
    import utils._

    val initName = newTermName("<init>")
    // Either:
    //   scala"{ var i = $low; val h = $hi; while (i < h) { $f(i); i = i + 1 } }
    // or:
    //   scala"($_this: RangeDefault).foreach($f)"
    _this match {
      case Apply(Select(New(tpt), initName), List(lo, hi)) if tpt.symbol.fullName == "Range" =>
        val iname = newTermName("$i")
        val hname = newTermName("$h")
        def iref = Ident(iname)
        def href = Ident(hname)
        val labelname = newTermName("$while")
        val cond = makeBinop(iref, "$less", href)
        val body = Block(
            List(makeApply(f, List(iref))),
            Assign(iref, makeBinop(iref, "$plus", Literal(Constant(1)))))
        val generated =
        Block(
          List(
            ValDef(Modifiers(Set(Modifier.mutable)), iname, TypeTree(), lo),
            ValDef(Modifiers(), hname, TypeTree(), hi)),
          makeWhile(labelname, cond, body))
        // todo. read the compiler config and print if -Ydebug is set
        //tools.nsc.util.trace("generated: ")(generated)
        generated
      case _ =>
        Apply(
          Select(
            Typed(_this, Ident(newTypeName("RangeDefault"))),
            newTermName("foreach")),
          List(f))
    }
  }
}

object Test extends App {

  new Range(1, 10) foreach println

}
