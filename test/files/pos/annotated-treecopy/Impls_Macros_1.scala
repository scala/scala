import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import collection.mutable.ListBuffer
import collection.mutable.Stack

object Macros {
  trait TypedFunction {
    def tree: scala.reflect.runtime.universe.Tree
    val typeIn: String
    val typeOut: String
  }

  def tree[T,U](f:Function1[T,U]): Function1[T,U] = macro tree_impl[T,U]

  def tree_impl[T:c.WeakTypeTag,U:c.WeakTypeTag](c: Context)
      (f:c.Expr[Function1[T,U]]): c.Expr[Function1[T,U]] = {
    import c.universe._
    import internal._
    val ttag = c.weakTypeTag[U]
    f match {
      case Expr(Function(List(ValDef(_,n,tp,_)),b)) =>
        // normalize argument name
        var b1 = new Transformer {
          override def transform(tree: Tree): Tree = tree match {
            case Ident(x) if (x==n) => Ident(TermName("_arg"))
            case tt: TypeTree if tt.original != null => setOriginal(TypeTree(tt.tpe), transform(tt.original))
            // without the fix to LazyTreeCopier.Annotated, we would need to uncomment the line below to make the macro work
            // that's because the pattern match in the input expression gets expanded into Typed(<x>, TypeTree(<Int @unchecked>))
            // with the original of the TypeTree being Annotated(<@unchecked>, Ident(<x>))
            // then the macro tries to replace all Ident(<x>) trees with Ident(<_arg>), recurs into the original of the TypeTree, changes it,
            // but leaves the <@unchecked> part untouched. this signals the misguided LazyTreeCopier that the Annotated tree hasn't been modified,
            // so the original tree should be copied over and returned => crash when later <x: @unchecked> re-emerges from TypeTree.original
            // case Annotated(annot, arg) => treeCopy.Annotated(tree, transform(annot).duplicate, transform(arg))
            case _ => super.transform(tree)
          }
        }.transform(b)

        val reifiedTree = c.reifyTree(gen.mkRuntimeUniverseRef, EmptyTree, b1)
        val reifiedExpr = c.Expr[scala.reflect.runtime.universe.Expr[T => U]](reifiedTree)
        val template =
          c.universe.reify(new (T => U) with TypedFunction {
            override def toString = c.Expr[String](q"""${tp+" => "+ttag.tpe+" { "+b1.toString+" } "}""").splice // DEBUG
            def tree = reifiedExpr.splice.tree
            val typeIn = c.Expr[String](q"${tp.toString}").splice
            val typeOut = c.Expr[String](q"${ttag.tpe.toString}").splice
            def apply(_arg: T): U = c.Expr[U](b1)(ttag.asInstanceOf[c.WeakTypeTag[U]]).splice
          })
        val untyped = c.untypecheck(template.tree)

        c.Expr[T => U](untyped)
      case _ => sys.error("Bad function type")
    }
  }
}