/* NSC -- new Scala compiler
 * Copyright 2005-2006 LAMP/EPFL
 * @author Martin Odersky
 */
// $Id$

package scala.tools.nsc.transform

import symtab._
import Flags._
import scala.collection.mutable.{HashMap, ListBuffer}
import matching.{TransMatcher, PatternNodes, CodeFactory, PatternMatchers}

/** This class ...
 *
 *  @author  Martin Odersky
 *  @version 1.0
 */
abstract class ExplicitOuter extends InfoTransform with TransMatcher with PatternNodes with CodeFactory with PatternMatchers with TypingTransformers {
  import global._
  import definitions._
  import posAssigner.atPos

  /** The following flags may be set by this phase: */
  override def phaseNewFlags: long = notPRIVATE | notPROTECTED

  /** the name of the phase: */
  val phaseName: String = "explicitouter"

  /** This class does not change linearization */
  override def changesBaseClasses = false

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitOuterTransformer(unit)

  /** Is given <code>clazz</code> an inner class? */
  private def isInner(clazz: Symbol) =
    !clazz.isPackageClass && !clazz.outerClass.isStaticOwner

  private def outerField(clazz: Symbol): Symbol = {
    val result = clazz.info.member(nme.getterToLocal(nme.OUTER))
    if (result == NoSymbol)
      assert(false, "no outer field in "+clazz+clazz.info.decls+" at "+phase)
    result
  }

  def outerAccessor(clazz: Symbol): Symbol = {
    val firstTry = clazz.info.decl(clazz.expandedName(nme.OUTER))
    if (firstTry != NoSymbol && firstTry.outerSource == clazz) firstTry
    else {
      var e = clazz.info.decls.elems
      while ((e ne null) && e.sym.outerSource != clazz) e = e.next
      if (e ne null) e.sym else NoSymbol
    }
  }

  /** <p>
   *    The type transformation method:
   *  </p>
   *  <ol>
   *    <li>
   *      Add an outer parameter to the formal parameters of a constructor
   *      in a inner non-trait class;
   *    </li>
   *    <li>
   *      Add a protected <code>$outer</code> field to an inner class which is
   *      not a trait.
   *    </li>
   *    <li>
   *      <p>
   *        Add an outer accessor <code>$outer$$C</code> to every inner class
   *        with fully qualified name <code>C</code> that is not an interface.
   *        The outer accesssor is abstract for traits, concrete for other
   *        classes.
   *      </p>
   *      <p>
   *        3a. Also add overriding accessor defs to every class that inherits
   *        mixin classes with outer accessor defs (unless the superclass
   *        already inherits the same mixin).
   *      </p>
   *    </li>
   *    <li>
   *      Add a mixin constructor <code>$init$</code> to all mixins except interfaces
   *      Leave all other types unchanged. todo: move to later
   *    </li>
   *    <li>
   *      Make all super accessors and modules in traits non-private, mangling
   *      their names.
   *    </li>
   *    <li>
   *      Remove protected flag from all members of traits.
   *    </li>
   *  </ol>
   */
  def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case MethodType(formals, restpe1) =>
      val restpe = transformInfo(sym, restpe1) // removeOption
      if (sym.owner.isTrait && ((sym hasFlag SUPERACCESSOR) || sym.isModule)) { // 5
        sym.makeNotPrivate(sym.owner)
      }
      if (sym.owner.isTrait && (sym hasFlag PROTECTED)) sym setFlag notPROTECTED // 6
      if (sym.isClassConstructor && isInner(sym.owner)) // 1
        MethodType(sym.owner.outerClass.thisType :: formals, restpe)
      else if (restpe ne restpe1)
        MethodType(formals, restpe) // removeOption
      else tp
    case ClassInfoType(parents, decls, clazz) =>
      var decls1 = decls
      if (isInner(clazz) && !(clazz hasFlag INTERFACE)) {
        decls1 = newScope(decls.toList)
        val outerAcc = clazz.newMethod(clazz.pos, nme.OUTER) // 3
        outerAcc.expandName(clazz)
        val restpe = if (clazz.isTrait) clazz.outerClass.tpe else clazz.outerClass.thisType
        decls1 enter clazz.newOuterAccessor(clazz.pos).setInfo(MethodType(List(), restpe))
        if (!clazz.isTrait) // 2
          //todo: avoid outer field if superclass has same outer value?
          decls1 enter (
            clazz.newValue(clazz.pos, nme.getterToLocal(nme.OUTER))
            setFlag (SYNTHETIC | PROTECTED | PARAMACCESSOR)
            setInfo clazz.outerClass.thisType)
      }
      if (!clazz.isTrait && !parents.isEmpty) {
        for (val mc <- clazz.mixinClasses) {
          val mixinOuterAcc: Symbol = atPhase(phase.next)(outerAccessor(mc))
          if (mixinOuterAcc != NoSymbol) {
            if (decls1 eq decls) decls1 = newScope(decls.toList)
            val newAcc = mixinOuterAcc.cloneSymbol(clazz)
            newAcc.resetFlag(DEFERRED)
            newAcc.setInfo(clazz.thisType.memberType(mixinOuterAcc))
            decls1 enter newAcc
          }
        }
      }
      if (decls1 eq decls) tp else ClassInfoType(parents, decls1, clazz)
    case PolyType(tparams, restp) =>
      val restp1 = transformInfo(sym, restp)
      if (restp eq restp1) tp else PolyType(tparams, restp1)

    // <removeOption>
    case TypeRef(_, sym, List(tpe))
    if  settings.Xkilloption.value
    && (sym == definitions.OptionClass || sym == definitions.SomeClass)
    && isSubType(tpe, definitions.AnyRefClass.tpe) =>
      tpe
    // </removeOption>

    case _ =>
      tp
  }

  /** A base class for transformers that maintain <code>outerParam</code>
   *  values for outer parameters of constructors.
   *  The class provides methods for referencing via outer.
   */
  abstract class OuterPathTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    /** The directly enclosing outer parameter, if we are in a constructor */
    protected var outerParam: Symbol = NoSymbol

    /** The first outer selection from currently transformed tree.
     *  The result is typed but not positioned.
     */
    protected def outerValue: Tree =
      if (outerParam != NoSymbol) gen.mkAttributedIdent(outerParam)
      else outerSelect(gen.mkAttributedThis(currentClass))

    /** Select and apply outer accessor from 'base'
     *  The result is typed but not positioned.
     */
    private def outerSelect(base: Tree): Tree =
      localTyper.typed(Apply(Select(base, outerAccessor(base.tpe.symbol)), List()))

    /** The path
     *  <blockquote><pre>`base'.$outer$$C1 ... .$outer$$Cn</pre></blockquote>
     *  which refers to the outer instance of class <code>to</code> of
     *  value <code>base</code>. The result is typed but not positioned.
     *
     *  @param base ...
     *  @param from ...
     *  @param to   ...
     *  @return     ...
     */
    protected def outerPath(base: Tree, from: Symbol, to: Symbol): Tree = {
      //Console.println("outerPath from "+from+" to "+to+" at "+base+":"+base.tpe)
      //assert(base.tpe.widen.baseType(from.toInterface) != NoType, ""+base.tpe.widen+" "+from.toInterface)//DEBUG
      if (from == to || from.isImplClass && from.toInterface == to) base
      else outerPath(outerSelect(base), from.outerClass, to)
    }

    override def transform(tree: Tree): Tree = {
      val savedOuterParam = outerParam
      try {
        tree match {
          case Template(_, _) =>
            outerParam = NoSymbol
          case DefDef(_, _, _, vparamss, _, _) =>
            if (tree.symbol.isClassConstructor && isInner(tree.symbol.owner)) {
              outerParam = vparamss.head.head.symbol
              assert(outerParam.name == nme.OUTER)
            }
          case _ =>
        }
        super.transform(tree)
      } catch {//debug
        case ex: Throwable =>
          Console.println("exception when transforming " + tree)
          throw ex
      } finally {
        outerParam = savedOuterParam
      }
    }
  }

  /** <p>
   *    The phase performs the following transformations on terms:
   *  </p>
   *  <ol>
   *    <li> <!-- 1 -->
   *      <p>
   *        An class which is not an interface and is not static gets an outer
   *        accessor (@see outerDefs).
   *      </p>
   *      <p>
   *        1a. A class which is not a trait gets an outer field.
   *      </p>
   *    </li>
   *    <li> <!-- 2 -->
   *      A mixin which is not also an interface gets a mixin constructor
   *      (@see mixinConstructorDef)
   *    </li>
   *    <li> <!-- 3 -->
   *      Constructor bodies are augmented by calls to supermixin constructors
   *      (@see addMixinConstructorCalls)
   *    </li>
   *    <li> <!-- 4 -->
   *      A constructor of a non-trait inner class gets an outer parameter.
   *    </li>
   *    <li> <!-- 5 -->
   *      A reference <code>C.this</code> where <code>C</code> refers to an
   *      outer class is replaced by a selection
   *      <code>this.$outer$$C1</code> ... <code>.$outer$$Cn</code> (@see outerPath)
   *    </li>
   *    <li>
   *    </li>
   *    <li> <!-- 7 -->
   *      A call to a constructor Q.<init>(args) or Q.$init$(args) where Q != this and
   *      the constructor belongs to a non-static class is augmented by an outer argument.
   *      E.g. <code>Q.&lt;init&gt;(OUTER, args)</code> where <code>OUTER</code>
   *      is the qualifier corresponding to the singleton type <code>Q</code>.
   *    </li>
   *    <li>
   *      A call to a constructor <code>this.&lt;init&gt;(args)</code> in a
   *      secondary constructor is augmented to <code>this.&lt;init&gt;(OUTER, args)</code>
   *      where <code>OUTER</code> is the last parameter of the secondary constructor.
   *    </li>
   *    <li> <!-- 9 -->
   *      Remove <code>private</code> modifier from class members <code>M</code>
   *      that are accessed from an inner class.
   *    </li>
   *    <li> <!-- 10 -->
   *      Remove <code>protected</code> modifier from class members <code>M</code>
   *      that are accessed without a super qualifier accessed from an inner
   *      class or trait.
   *    </li>
   *    <li> <!-- 11 -->
   *      Remove <code>private</code> and <code>protected</code> modifiers
   *      from type symbols
   *    </li>
   *    <li> <!-- 12 -->
   *      Remove <code>private</code> modifiers from members of traits
   *    </li>
   *  </ol>
   *  <p>
   *    Note: The whole transform is run in phase <code>explicitOuter.next</code>.
   *  </p>
   */
  class ExplicitOuterTransformer(unit: CompilationUnit) extends OuterPathTransformer(unit) {

    /** The definition tree of the outer accessor of current class
     */
    def outerFieldDef: Tree = {
      val outerF = outerField(currentClass)
      ValDef(outerF, EmptyTree)
    }

    /** The definition tree of the outer accessor of current class
     */
    def outerAccessorDef: Tree = {
      val outerAcc = outerAccessor(currentClass)
      var rhs = if (outerAcc hasFlag DEFERRED) EmptyTree
                else Select(This(currentClass), outerField(currentClass))
      localTyper.typed {
        atPos(currentClass.pos) {
          DefDef(outerAcc, vparamss => rhs)
        }
      }
    }

    /** The definition tree of the outer accessor for class
     * <code>mixinClass</code>.
     *
     *  @param mixinClass The mixin class which defines the abstract outer
     *                    accessor which is implemented by the generated one.
     *  @pre mixinClass is an inner class
     */
    def mixinOuterAccessorDef(mixinClass: Symbol): Tree = {
      val outerAcc = outerAccessor(mixinClass).overridingSymbol(currentClass)
      if (outerAcc == NoSymbol)
        Console.println("cc " + currentClass + ":" + currentClass.info.decls +
                        " at " + phase)//debug
      assert(outerAcc != NoSymbol)
      val path = gen.mkAttributedQualifier(currentClass.thisType.baseType(mixinClass).prefix)
      val rhs = ExplicitOuterTransformer.this.transform(path)
      localTyper.typed {
        atPos(currentClass.pos) {
          DefDef(outerAcc, vparamss => rhs)
        }
      }
    }

    /** The main transformation method */
    override def transform(tree: Tree): Tree = {

      def wasNone(tpe:Type) = atPhase(phase.prev){tpe}.symbol == definitions.NoneClass
      def wasOption(tpe:Type) = {
        def ntpe = if(tpe.isInstanceOf[SingleType]) tpe.widen else tpe
        definitions.isOptionType(atPhase(phase.prev){ntpe})
      }
      def wasOptionRef(tpe:Type) = {
        def ntpe = if(tpe.isInstanceOf[SingleType]) tpe.widen else tpe
        val otpe = atPhase(phase.prev){ntpe}
        definitions.isOptionType(otpe) && isSubType(otpe.typeArgs(0), definitions.AnyRefClass.tpe)
      }

      val sym = tree.symbol
      if ((sym ne null) && sym.isType) {//(9)
        if (sym hasFlag PRIVATE) sym setFlag notPRIVATE
        if (sym hasFlag PROTECTED) sym setFlag notPROTECTED
      }
      tree match {

        // <removeOption>

        case sm @ Apply(fn, List(vlue)) // scala.Some(x:T) -> x  if T <: AnyRef
        if settings.Xkilloption.value
        && definitions.isSomeType(sm.tpe)
        && fn.symbol.isClassConstructor
        && isSubType(sm.tpe.typeArgs(0), definitions.AnyRefClass.tpe) =>
          vlue


        case nne: Select                                // scala.None -> null
        if settings.Xkilloption.value && wasNone(nne.tpe) =>
          atPos(tree.pos) { typer.typed { Literal(Constant(null)) }}

        case Apply(Select(t, nme.isEmpty),List())       // t.isEmpty -> t eq null
        if settings.Xkilloption.value && wasOption(t.tpe)  =>
          IsNull(t)

        case Apply(Select(t, nme.get),List())           // t.get -> t if T <: Option[AnyRef]
        if settings.Xkilloption.value && wasOptionRef(t.tpe) =>
          t

        case Select(t, n)           // methods ...
        if settings.Xkilloption.value && wasOption(t.tpe) && n != nme.get =>
          def ntpe = if(t.tpe.isInstanceOf[SingleType]) t.tpe.widen else t.tpe
          val otpe = atPhase(phase.prev){ntpe}
          val nt = atPos(tree.pos) { typer.typed {
            If(NotNull(t),
               Apply(Select(New(TypeTree( definitions.someType(otpe.typeArgs(0)))), nme.CONSTRUCTOR), List(t)),
               gen.mkAttributedRef(definitions.NoneClass))
          }}
	  copy.Select(tree,nt,n)

        case _                                   // e.g. map.get("bar") -> fix type
        if settings.Xkilloption.value
        && wasOptionRef(tree.tpe) =>
          super.transform(tree.setType(atPhase(phase.prev){tree.tpe}.typeArgs(0)))

        // </removeOption>

        case Template(parents, decls) =>
          val newDefs = new ListBuffer[Tree]
          atOwner(tree, currentOwner) {
            if (!(currentClass hasFlag INTERFACE) || (currentClass hasFlag lateINTERFACE)) {
              if (isInner(currentClass)) {
                if (!currentClass.isTrait) newDefs += outerFieldDef // (1a)
                newDefs += outerAccessorDef // (1)
              }
              if (!currentClass.isTrait)
                for (val mc <- currentClass.mixinClasses)
                  if (outerAccessor(mc) != NoSymbol)
                    newDefs += mixinOuterAccessorDef(mc)
            }
          }
          super.transform(
            copy.Template(tree, parents, if (newDefs.isEmpty) decls else decls ::: newDefs.toList))

        case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
          if (sym.isClassConstructor) {
            rhs match {
              case Literal(_) =>
                // replace unit rhs () by empty block {()}
                val rhs1 = Block(List(), rhs) setPos rhs.pos setType rhs.tpe
                transform(copy.DefDef(tree, mods, name, tparams, vparamss, tpt, rhs1))
              case _ =>
                val clazz = sym.owner
                val vparamss1 =
                  if (isInner(clazz)) { // (4)
                    val outerParam =
                      sym.newValueParameter(sym.pos, nme.OUTER) setInfo outerField(clazz).info
                    ((ValDef(outerParam) setType NoType) :: vparamss.head) :: vparamss.tail
                  } else vparamss
                super.transform(copy.DefDef(tree, mods, name, tparams, vparamss1, tpt, rhs))
            }
          } else { //todo: see whether we can move this to transformInfo
            if (sym.owner.isTrait && (sym hasFlag (ACCESSOR | SUPERACCESSOR)))
              sym.makeNotPrivate(sym.owner); //(2)
            if(settings.Xkilloption.value
               && definitions.isOptionType(tpt.tpe)
               && isSubType(tpt.tpe.typeArgs(0), definitions.AnyRefClass.tpe)) {

                 return super.transform(copy.DefDef(tree,mods,name,tparams,vparamss, TypeTree(tpt.tpe.typeArgs(0)), rhs))
               }
            super.transform(tree)
          }

        case This(qual) =>
          if (sym == currentClass || (sym hasFlag MODULE) && sym.isStatic) tree
          else atPos(tree.pos)(outerPath(outerValue, currentClass.outerClass, sym)) // (5)

        case Select(qual, name) =>
          if (currentClass != sym.owner && currentClass != sym.moduleClass) // (3)
            sym.makeNotPrivate(sym.owner)
          val qsym = qual.tpe.widen.symbol
          if ((sym hasFlag PROTECTED) && //(4)
              (qsym.isTrait || !(qual.isInstanceOf[Super] || (qsym isSubClass currentClass))))
            sym setFlag notPROTECTED
          super.transform(tree)

        case Apply(sel @ Select(qual, name), args)
          if (name == nme.CONSTRUCTOR && isInner(sel.symbol.owner)) =>
            val outerVal = atPos(tree.pos) {
              if (qual.isInstanceOf[This]) { // it's a call between constructors of same class
                assert(outerParam != NoSymbol)
                outerValue
              } else {
                var pre = qual.tpe.prefix
                if (pre == NoPrefix) pre = sym.owner.outerClass.thisType
                gen.mkAttributedQualifier(pre)
              }
            }
            super.transform(copy.Apply(tree, sel, outerVal :: args))

        case Match(selector, cases) => // <----- transmatch hook
          val tid = if (settings.debug.value) {
            val q = unit.fresh.newName("tidmark")
            Console.println("transforming patmat with tidmark "+q+" ncases = "+cases.length)
            q
          } else null

          cases match {
            //if ((cases.length > 1) && ...(cases(0)))
            //can't use treeInfo.isDefaultCase, because that diagnoses a Bind
            case CaseDef(Ident(nme.WILDCARD), EmptyTree, _)::xs if !xs.isEmpty =>
              // a hack to detect when explicit outer does not work correctly
              // still needed?
              assert(false,"transforming too much, " + tid)
            case _ =>
          }

          val nselector = transform(selector)
          assert(nselector.tpe =:= selector.tpe)
          val ncases = transformCaseDefs(cases)

          ExplicitOuter.this.resultType = tree.tpe
          //Console.println("TransMatcher currentOwner ="+currentOwner+")")
          //Console.println("TransMatcher selector.tpe ="+selector.tpe+")")
          //Console.println("TransMatcher resultType ="+resultType+")")

          val t_untyped = handlePattern(nselector, ncases, currentOwner, transform)
          //Console.println("t_untyped "+t_untyped.toString())
          val t = atPos(tree.pos) { localTyper.typed(t_untyped, resultType) }

          //t = transform(t)
          //val t         = atPos(tree.pos) { typed(t_untyped, resultType) }
          //val t         = atPos(tree.pos) { typed(t_untyped) }
          //Console.println("t typed "+t.toString())
          if (settings.debug.value)
            Console.println("finished translation of " + tid)
          t

        case _ =>
          super.transform(tree)
      }
    }

    /** The transformation method for whole compilation units */
    override def transformUnit(unit: CompilationUnit): unit = {
      cunit = unit
      atPhase(phase.next) { super.transformUnit(unit) }
    }
  }
}


