import annotation.tailrec
import scala.math.{min, max}
import scala.{specialized => spec}

import language.experimental.macros

import scala.reflect.ClassTag
import scala.reflect.macros.blackbox.Context

object Macros {
  def alloc[@spec A:ClassTag](src:Array[A], s1:Int, len:Int) = {
    val as = Array.ofDim[A](len)
    System.arraycopy(src, s1, as, 0, len)
    as
  }

  /**
   * Efficient alternative to Array.apply.
   *
   * "As seen on scala-internals!"
   */
  def array[A](as:A*)(implicit ct: ClassTag[A]) = macro arrayMacro[A]

  /**
   * Takes in something like:
   *   ArrayUtil.alloc[Int](11, 22, 33, 44)(ct)
   *
   * and builds a tree like:
   *   {
   *     val arr:Array[Int] = ct.newArray(4)
   *     arr.update(0, 11)
   *     arr.update(1, 22)
   *     arr.update(2, 33)
   *     arr.update(3, 44)
   *     arr
   *   }
   */
  def arrayMacro[A:c.WeakTypeTag](c:Context)(as:c.Expr[A]*)(ct: c.Expr[ClassTag[A]]): c.Expr[Array[A]] = {
    import c.mirror._
    import c.universe._
    def const(x:Int) = Literal(Constant(x))

    val n = as.length
    val arr = TermName("arr")

    val create = Apply(Select(ct.tree, TermName("newArray")), List(const(n)))
    val arrtpe = TypeTree(implicitly[c.WeakTypeTag[Array[A]]].tpe)
    val valdef = ValDef(Modifiers(), arr, arrtpe, create)

    val updates = (0 until n).map {
      i => Apply(Select(Ident(arr), TermName("update")), List(const(i), as(i).tree))
    }

    val exprs = (Seq(valdef) ++ updates ++ Seq(Ident(arr))).toList
    val block = Block(exprs.init, exprs.last)

    c.Expr[Array[A]](block)
  }
}