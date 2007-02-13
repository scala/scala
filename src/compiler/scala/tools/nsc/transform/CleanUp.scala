/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.tools.nsc.util.Position
import scala.collection.mutable.{ListBuffer, HashMap}

abstract class CleanUp extends Transform {
  import global._
  import definitions._
  import posAssigner.atPos

  /** the following two members override abstract members in Transform */
  val phaseName: String = "cleanup"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CleanUpTransformer(unit)

  class CleanUpTransformer(unit: CompilationUnit) extends Transformer {

    private val newDefs = new ListBuffer[Tree]
    private val classConstantMeth = new HashMap[String, Symbol]

    // a map from the symbols of the Scala primitive types to the symbols
    // of the modules of the Java box classes
    private val javaBoxClassModule = new HashMap[Symbol, Symbol]

    if (!forMSIL) {
      javaBoxClassModule(UnitClass)    = getModule("java.lang.Void")
      javaBoxClassModule(BooleanClass) = getModule("java.lang.Boolean")
      javaBoxClassModule(ByteClass)    = getModule("java.lang.Byte")
      javaBoxClassModule(ShortClass)   = getModule("java.lang.Short")
      javaBoxClassModule(IntClass)     = getModule("java.lang.Integer")
      javaBoxClassModule(CharClass)    = getModule("java.lang.Character")
      javaBoxClassModule(LongClass)    = getModule("java.lang.Long")
      javaBoxClassModule(FloatClass)   = getModule("java.lang.Float")
      javaBoxClassModule(DoubleClass)  = getModule("java.lang.Double")
    }

    private var localTyper: analyzer.Typer = null;

    private def freshClassConstantMethName() = unit.fresh.newName("class$Method")
    private def freshClassConstantVarName() = unit.fresh.newName("class$Cache")

    private def classConstantMethod(pos: PositionType, sig: String): Symbol = classConstantMeth.get(sig) match {
      case Some(meth) =>
        meth
      case None =>
        val forName = getMember(ClassClass.linkedModuleOfClass, nme.forName)
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
      if (settings.target.value != "jvm-1.5" && !forMSIL) {
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
              Select(gen.mkAttributedRef(javaBoxClassModule(tpe.symbol)), "TYPE")
            else
              Apply(
                gen.mkAttributedRef(classConstantMethod(tree.pos, signature(tpe))),
                List())
          }
        }
      case _ =>
        super.transform(tree)
    }
  } // CleanUpTransformer

}
