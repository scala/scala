/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.collection.mutable.{HashMap, ListBuffer}

abstract class ExplicitOuter extends InfoTransform {
  import global._
  import definitions._
  import posAssigner.atPos

  override def phaseNewFlags: long = notPRIVATE | notPROTECTED

  /** the following two members override abstract members in Transform */
  val phaseName: String = "explicitouter"
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitOuterTransformer(unit)

  private def outerClass(clazz: Symbol): Symbol =
    if (clazz.owner.isClass) clazz.owner
    else outerClass(if (clazz.isClassLocalToConstructor) clazz.owner.owner else clazz.owner)

  private def isStatic(clazz: Symbol) =
    clazz.isPackageClass || outerClass(clazz).isStaticOwner

  /** The type transformation method:
   *  1. Add an outer paramter to the formal parameters of a constructor or mixin constructor
   *     in a non-static class;
   *  2. Add a mixin constructor $init$ to all mixins except interfaces
   *  Leave all other types unchanged.
   */
  def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case MethodType(formals, restpe) =>
      //todo: needed?
      if (sym.owner.isTrait && (sym hasFlag SUPERACCESSOR))
        sym.makeNotPrivate(sym.owner);
      if (sym.owner.isTrait && (sym hasFlag PROTECTED)) sym setFlag notPROTECTED
      if (sym.isConstructor && !isStatic(sym.owner))
        MethodType(formals ::: List(outerClass(sym.owner).toInterface.thisType), restpe)
      else tp
    case ClassInfoType(parents, decls, clazz) =>
      var decls1 = decls
      if (!(clazz hasFlag INTERFACE)) {
        if (!isStatic(clazz)) {
          decls1 = newScope(decls1.toList)
          val outerAcc = clazz.newMethod(clazz.pos, nme.OUTER)
          if (clazz.isTrait || (decls.toList exists (.isClass)))
            outerAcc.expandName(clazz);
          decls1 enter (
            outerAcc setFlag (PARAMACCESSOR | ACCESSOR | STABLE)
                     setInfo MethodType(List(), outerClass(clazz).thisType));
          decls1 enter (clazz.newValue(clazz.pos, nme.getterToLocal(outerAcc.name))
            setFlag (LOCAL | PRIVATE | PARAMACCESSOR | (outerAcc getFlag EXPANDEDNAME))
            setInfo outerClass(clazz).thisType)
        }
        if (clazz.isTrait) {
          decls1 = newScope(decls1.toList)
          decls1 enter makeMixinConstructor(clazz)
        }
      }
      if (decls1 eq decls) tp else ClassInfoType(parents, decls1, clazz)
    case PolyType(tparams, restp) =>
      val restp1 = transformInfo(sym, restp)
      if (restp eq restp1) tp else PolyType(tparams, restp1)
    case _ =>
      tp
  }

  private def outerMember(tp: Type): Symbol = {
    var e = tp.symbol.info.decls.elems
      // note: tp.decls does not work here, because tp might be a ThisType, in which case
      // its decls would be the decls of the required type of the class.
    while (e != null && !(e.sym.originalName.startsWith(nme.OUTER) && (e.sym hasFlag ACCESSOR)))
      e = e.next;
    assert(e != null, tp)
    e.sym
  }

  private def makeMixinConstructor(clazz: Symbol): Symbol =
    clazz.newMethod(clazz.pos, nme.MIXIN_CONSTRUCTOR) setInfo MethodType(List(), UnitClass.tpe)

  /** A base class for transformers that maintain `outerParam' values for
   *  outer parameters of constructors.
   *  The class provides methods for referencing via outer.
   */
  class OuterPathTransformer extends Transformer {

    /** The directly enclosing outer parameter, if we are in a constructor */
    protected var outerParam: Symbol = NoSymbol

    /** The first outer selection from currently transformed tree
     */
    protected def outerValue: Tree =
      if (outerParam != NoSymbol) gen.mkAttributedIdent(outerParam)
      else outerSelect(gen.mkAttributedThis(currentOwner.enclClass))

    /** The path
     *     `base'.$outer ... .$outer
     *  which refers to the outer instance 'to' of value 'base'
     */
    protected def outerPath(base: Tree, to: Symbol): Tree =
      if (base.tpe.symbol == to) base else outerPath(outerSelect(base), to)

    /** Select and apply outer accessor from 'base'
     */
    private def outerSelect(base: Tree): Tree = {
      val otp = outerClass(base.tpe.symbol).thisType
      Apply(
        Select(base, outerMember(base.tpe)) setType MethodType(List(), otp),
        List()) setType otp
    }

    override def transform(tree: Tree): Tree = {
      try {//debug
        val savedOuterParam = outerParam;
        tree match {
          case Template(_, _) =>
            outerParam = NoSymbol
          case DefDef(_, _, _, vparamss, _, _) =>
            if (tree.symbol.isConstructor && !(isStatic(tree.symbol.owner))) {
              val lastParam = vparamss.head.last
              assert(lastParam.name.startsWith(nme.OUTER), tree)
              outerParam = lastParam.symbol
            }
          case _ =>
        }
        val result = super.transform(tree);
        outerParam = savedOuterParam
        result
      } catch {//debug
        case ex: Throwable =>
          System.out.println("exception when transforming " + tree)
          throw ex
      }
    }
  }

  class ExplicitOuterTransformer(unit: CompilationUnit) extends Transformer {

    /** The first step performs the following transformations:
     *   1. A class which is not an interface and is not static gets an outer link
     *      (@see outerDefs)
     *   2. A mixin which is not also an interface gets a mixin constructor
     *      (@see mixinConstructorDef)
     *   3. Constructor bodies are augmented by calls to supermixin constructors
     *      (@see addMixinConstructorCalls)
     *   4. A constructor of a class with an outer link gets an outer parameter.
     *   5. A reference C.this where C refers to an outer class is replaced by a selection
     *        this.$outer ... .$outer (@see outerPath)
     *   7. A call to a constructor Q.<init>(args) or Q.$init$(args) where Q != this and
     *      the constructor belongs to a non-static class is augmented by an outer argument.
     *      E.g. Q.<init>(args, OUTER) where OUTER is the qualifier corresponding to the
     *      singleton type Q.
     *   8. A call to a constructor this.<init>(args) in a secondary constructor
     *      is augmented to this.<init>(args, OUTER) where OUTER is the last parameter
     *      of the secondary constructor.
     */
    private val firstTransformer = new OuterPathTransformer {

      var localTyper: analyzer.Typer = typer

      /** The two definitions
       *    val outer: C.this.type _;
       *    def outer(): C.this.type  = outer ;
       *  Here, C is the class enclosing the class `clazz' containing the two definitions.
       */
      def outerDefs(clazz: Symbol): List[Tree] = {
        val outerDef = outerMember(clazz.info)
        val outerVal = outerDef.accessed
        List(
          localTyper.typed {
            atPos(clazz.pos) {
              ValDef(outerVal)
            }
          },
          localTyper.typed {
            atPos(clazz.pos) {
              DefDef(outerDef, vparamss => Select(This(clazz), outerVal))
            }
          })
      }

      /** The mixin constructor definition
       *    def $init$(): Unit = ()
       */
      def mixinConstructorDef(clazz: Symbol): Tree =
        localTyper.typed {
          val constr = clazz.primaryConstructor
          atPhase(currentRun.explicitOuterPhase) {
            // necessary so that we do not include an outer parameter already here;
            // this will be added later in transform.
            DefDef(constr, vparamss => Literal(()))
          }
        }

      /** Add calls to supermixin constructors
       *     super[mix].$init$()
       *  to `tree'. `tree' which is assumed to be the body of a constructor of class `clazz'.
       */
      def addMixinConstructorCalls(tree: Tree, clazz: Symbol): Tree = {
        def mixinConstructorCall(mixinClass: Symbol): Tree =
          atPos(tree.pos) {
            Apply(
              localTyper.typedOperator {
                Select(This(clazz), mixinClass.primaryConstructor)
              },
              List()) setType UnitClass.tpe; // don't type this with typed(...),
                                              // as constructor arguments might be missing
          }
        /*
        val mixinConstructorCalls =
          for (val mixinClass <- clazz.info.parents.tail;
               !(mixinClass.symbol hasFlag INTERFACE) && mixinClass.symbol != ScalaObjectClass) yield
            mixinConstructorCall(mixinClass.symbol);
        tree match {
        */
        val mixinConstructorCalls: List[Tree] = {
          val ps = clazz.info.parents
          if (ps.isEmpty) List()
          else {
            val superClass = ps.head.symbol
            for {
              val mclazz <- clazz.info.baseClasses.tail.takeWhile(superClass ne).reverse
              mclazz.needsImplClass && mclazz != ScalaObjectClass
            } yield mixinConstructorCall(mclazz)
          }
        }

        //val result =
        tree match {
          case Block(supercall :: stats, expr) =>
            assert(supercall match {
              case Apply(Select(Super(_, _), _), _) => true
              case _ => false
            })
            copy.Block(tree, supercall :: mixinConstructorCalls ::: stats, expr)
          case Block(_, _) =>
            assert(false, tree);  tree
        }

        //System.out.println("new constructor of "+clazz+": "+result);
        //result
      }

      /** The first-step transformation method */
      override def transform(tree: Tree): Tree = {
        val sym = tree.symbol
        val tree1 = tree match {
          case Template(parents, decls) =>
            val savedLocalTyper = localTyper
            localTyper = localTyper.atOwner(tree, currentOwner)
            var decls1 = decls
            if (!(currentOwner hasFlag INTERFACE) || (currentOwner hasFlag lateINTERFACE)) {
              if (!isStatic(currentOwner))
                decls1 = decls1 ::: outerDefs(currentOwner); // (1)
              if (currentOwner.isTrait)
                decls1 = decls1 ::: List(mixinConstructorDef(currentOwner)) // (2)
            }
            localTyper = savedLocalTyper
            copy.Template(tree, parents, decls1)
          case constrDef @ DefDef(mods, name, tparams, vparamss, tpt, rhs)
          if (sym.isConstructor) =>
            rhs match {
              case Literal(_) =>
                val rhs1 = Block(List(), rhs) setPos rhs.pos setType rhs.tpe;
                transform(copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))
              case _ =>
                val clazz = sym.owner.toInterface
                val vparamss1 =
                  if (isStatic(clazz)) vparamss
                  else { // (4)
                    val outerField = outerMember(clazz.info).accessed;
                    val outerParam = sym.newValueParameter(sym.pos, nme.OUTER) setInfo outerField.info;
                    List(vparamss.head ::: List(ValDef(outerParam) setType NoType))
                  }
                val rhs1 =
                  if (sym.isPrimaryConstructor && sym.isClassConstructor && clazz != ArrayClass)
                    addMixinConstructorCalls(rhs, clazz); // (3)
                  else rhs;
                copy.DefDef(tree, mods, name, tparams, vparamss1, tpt, rhs1)
            }
          case This(qual) =>
            if (sym == currentOwner.enclClass || (sym hasFlag MODULE) && sym.isStatic) tree
            else atPos(tree.pos)(outerPath(outerValue, sym)); // (5)
          case Apply(sel @ Select(qual, name), args)
          if ((name == nme.CONSTRUCTOR || name == nme.MIXIN_CONSTRUCTOR) && !isStatic(sel.symbol.owner)) =>
            val outerVal = atPos(tree.pos) {
              if (name == nme.MIXIN_CONSTRUCTOR) {
                val mclazz = sym.owner.toInterface
                //System.out.println("adding mixin constructor for " + currentOwner.enclClass + " " + mclazz + " " + currentOwner.enclClass.thisType.baseType(mclazz));//DEBUG
                var pre = currentOwner.enclClass.thisType.baseType(mclazz).prefix
                if (pre == NoPrefix) pre = outerClass(mclazz).thisType
                gen.mkAttributedQualifier(pre)
              } else if (qual.isInstanceOf[This]) {
                assert(outerParam != NoSymbol); outerValue
              } else {
                var pre = qual.tpe.prefix
                if (pre == NoPrefix) pre = outerClass(sym.owner).thisType;
                gen.mkAttributedQualifier(pre)
              }
            }
            copy.Apply(tree, sel, args ::: List(outerVal))
          case _ =>
            tree
        }
        super.transform(tree1)
      }
    }

    /** The second step performs the following transformations:
     *   2. Remove private modifiers from members M of mixins T. (@see makeNotPrivate)
     *   3. Remove `private' modifier from class members M that are accessed from an inner class.
     *   4. Remove `protected' modifier from class members M that are accessed
     *      without a super qualifier accessed from an inner class or trait.
     *   5. Remove `private' and `protected' modifiers from type symbols
     */
    private val secondTransformer = new Transformer {

      /** The second-step transformation method */
      override def transform(tree: Tree): Tree = {
        val sym = tree.symbol
        val tree1 = super.transform(tree)
        tree1 match {
          case DefDef(_, _, _, _, _, _) =>
            if (sym.owner.isTrait && (sym hasFlag (ACCESSOR | SUPERACCESSOR)))
              sym.makeNotPrivate(sym.owner); //(2)
            tree1
          case Select(qual, name) =>
            val enclClass = currentOwner.enclClass
            if (enclClass != sym.owner && enclClass != sym.moduleClass) // (3)
              sym.makeNotPrivate(sym.owner);
            val qsym = qual.tpe.widen.symbol
            if ((sym hasFlag PROTECTED) && //(4)
                (qsym.isTrait || !(qual.isInstanceOf[Super] || (qsym isSubClass enclClass))))
              sym setFlag notPROTECTED;
            tree1
          case _ =>
            if (sym != null && sym.isType) {//(5)
              if (sym hasFlag PRIVATE) sym setFlag notPRIVATE
              if (sym hasFlag PROTECTED) sym setFlag notPROTECTED
            }
            tree1
        }
      }
    }

    /** The main transformation method:
     *  First, perform step 1 on whole tree of compilation unit.
     *  Then, perform step 2 on resulting tree
     */
    override def transform(tree: Tree) =
      atPhase(phase.next) {
        secondTransformer.transform(firstTransformer.transform(tree))
      }
  }
}


