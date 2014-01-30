import scala.reflect.macros.blackbox.Context

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
  import context.universe._
  import internal._

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
        if (tree1 ne tree) setType(tree1, null)
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
    Apply(Select(left, TermName(op)), List(right))
}
