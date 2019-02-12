import scala.tools.nsc.Global

abstract class C {
  val global: Global
  import global._
  def f(t: Tree): List[Tree] =
    t match {
      case DefDef(mods, name, tparams, vparams, tpe, rhs) =>
        //mods.annotations ::: tpe :: rhs :: vparams.flatten :::[Tree] tparams
        //mods.annotations ::: tpe :: rhs :: vparams.flatten ::: tparams.asInstanceOf[List[Tree]]
        mods.annotations ::: tpe :: rhs :: vparams.flatten ::: tparams
    }
}


/* Motivating case for always lifting adapted arg instead of cherry-picking subexpressions.

  def f(t: C.this.global.Tree): List[C.this.global.Tree] = t match {
    case (mods: C.this.global.Modifiers, name: C.this.global.TermName, tparams: List[C.this.global.TypeDef], vparamss: List[List[C.this.global.ValDef]], tpt: C.this.global.Tree, rhs: C.this.global.Tree):
      C.this.global.DefDef((mods @ _), (name @ _), (tparams @ _), (vparams @ _), (tpe @ _), (rhs @ _)) => {
final <synthetic> <artifact> val rassoc$1: List[C.this.global.ValDef] = vparams.flatten[C.this.global.ValDef];
tparams.:::[C.this.global.MemberDef with java.io.Serializable{def name: C.this.global.Name{def newName(str: String): C.this.global.Name{type ThisNameType >: C.this.global.TypeName with C.this.global.TermName <: C.this.global.Name}; def subName(from: Int, to: Int): C.this.global.Name{type ThisNameType >: C.this.global.TypeName with C.this.global.TermName <: C.this.global.Name}; def companionName: C.this.global.Name{type ThisNameType >: C.this.global.TermName with C.this.global.TypeName <: C.this.global.Name}; def next: C.this.global.Name{type ThisNameType >: C.this.global.TypeName with C.this.global.TermName <: C.this.global.Name}; type ThisNameType >: C.this.global.TypeName with C.this.global.TermName <: C.this.global.Name{type ThisNameType >: C.this.global.TypeName with C.this.global.TermName <: C.this.global.Name}}}]
(rassoc$1(scala.Predef.$conforms[List[C.this.global.ValDef]]))
  .::[C.this.global.Tree](rhs).::[C.this.global.Tree](tpe).:::[C.this.global.Tree](mods.annotations)
                                    }
                                        }
*/
