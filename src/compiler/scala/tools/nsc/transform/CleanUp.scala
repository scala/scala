/* NSC -- new scala compiler
 * Copyright 2005 LAMP/EPFL
 * @author
 */
// $Id: Mixin.scala 7249 2006-04-25 16:01:59 +0200 (Tue, 25 Apr 2006) odersky $
package scala.tools.nsc.transform;

import symtab._;
import Flags._;
import scala.tools.nsc.util.Position;
import scala.collection.mutable.{ListBuffer, HashMap};

abstract class CleanUp extends Transform {
  import global._;
  import definitions._;
  import posAssigner.atPos;

  /** the following two members override abstract members in Transform */
  val phaseName: String = "cleanup";

  protected def newTransformer(unit: CompilationUnit): Transformer = new CleanUpTransformer(unit);

  class CleanUpTransformer(unit: CompilationUnit) extends Transformer {

    private val newDefs = new ListBuffer[Tree]
    private val classConstantMeth = new HashMap[String, Symbol]

    private var localTyper: analyzer.Typer = null;

    private def freshClassConstantMethName() = unit.fresh.newName("class$Method")
    private def freshClassConstantVarName() = unit.fresh.newName("class$Cache")

    private def classConstantMethod(pos: int, sig: String): Symbol = classConstantMeth.get(sig) match {
      case Some(meth) =>
        meth
      case None =>
        val forName = getMember(ClassClass.linkedModule, nme.forName)
        val owner = currentOwner.enclClass

        val cvar = owner.newVariable(pos, freshClassConstantVarName())
          .setFlag(PRIVATE | STATIC | MUTABLE | SYNTHETIC).setInfo(ClassClass.tpe)
        owner.info.decls.enter(cvar)
        val cdef =
          localTyper.typed {
            atPos(pos) {
              ValDef(cvar, Literal(Constant(null)))
            }
          }

        val meth = owner.newMethod(pos, freshClassConstantMethName())
          .setFlag(PRIVATE | STATIC | SYNTHETIC).setInfo(MethodType(List(), ClassClass.tpe))
        owner.info.decls.enter(meth)
        val mdef =
          localTyper.typed {
            atPos(pos) {
              DefDef(meth, vparamss =>
                gen.mkCached(
                  cvar,
                  Apply(
                    gen.mkAttributedRef(forName), List(Literal(sig)))))
            }
          }

        newDefs.append(cdef, mdef);
        classConstantMeth.update(sig, meth)
        meth
    }

    override def transformUnit(unit: CompilationUnit) =
      if (settings.target.value != "jvm-1.5") {
        unit.body = transform(unit.body)
      }

    override def transform(tree: Tree): Tree = tree match {
      case Template(parents, body) =>
        classConstantMeth.clear
        newDefs.clear
        localTyper = typer.atOwner(tree, currentOwner)
        val body1 = transformTrees(body)
        copy.Template(tree, parents, newDefs.toList ::: body1)
      case Literal(c) if (c.tag == ClassTag) =>
        val tpe = c.typeValue
        atPos(tree.pos) {
          localTyper.typed {
            if (isValueClass(tpe.symbol))
              gen.mkRuntimeCall(tpe.symbol.name.toString() + "TYPE", List())
            else
              Apply(
                gen.mkAttributedRef(classConstantMethod(tree.pos, signature(tpe))),
                List())
          }
        }
      case _ =>
        super.transform(tree)
    }
  }
}
