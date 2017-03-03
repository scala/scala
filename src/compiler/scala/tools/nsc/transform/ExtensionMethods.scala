/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.mutable

/**
 * Perform Step 1 in the inline classes SIP: Creates extension methods for all
 * methods in a value class, except parameter or super accessors, or constructors.
 *
 *  @author Martin Odersky
 *  @version 2.10
 */
abstract class ExtensionMethods extends Transform with TypingTransformers {

  import global._ // the global environment
  import definitions._ // standard classes and methods

  /** the following two members override abstract members in Transform */
  val phaseName: String = "extmethods"

  def newTransformer(unit: CompilationUnit): Transformer =
    new Extender(unit)

  /** Generate stream of possible names for the extension version of given instance method `imeth`.
   *  If the method is not overloaded, this stream consists of just "extension$imeth".
   *  If the method is overloaded, the stream has as first element "extensionX$imeth", where X is the
   *  index of imeth in the sequence of overloaded alternatives with the same name. This choice will
   *  always be picked as the name of the generated extension method.
   *  After this first choice, all other possible indices in the range of 0 until the number
   *  of overloaded alternatives are returned. The secondary choices are used to find a matching method
   *  in `extensionMethod` if the first name has the wrong type. We thereby gain a level of insensitivity
   *  of how overloaded types are ordered between phases and picklings.
   */
  private def extensionNames(imeth: Symbol): Stream[Name] = {
    val decl = imeth.owner.info.decl(imeth.name)

    // Bridge generation is done at phase `erasure`, but new scopes are only generated
    // for the phase after that. So bridges are visible in earlier phases.
    //
    // `info.member(imeth.name)` filters these out, but we need to use `decl`
    // to restrict ourselves to members defined in the current class, so we
    // must do the filtering here.
    val declTypeNoBridge = decl.filter(sym => !sym.isBridge).tpe

    declTypeNoBridge match {
      case OverloadedType(_, alts) =>
        val index = alts indexOf imeth
        assert(index >= 0, alts+" does not contain "+imeth)
        def altName(index: Int) = newTermName(imeth.name+"$extension"+index)
        altName(index) #:: ((0 until alts.length).toStream filter (index != _) map altName)
      case tpe =>
        assert(tpe != NoType, imeth.name+" not found in "+imeth.owner+"'s decls: "+imeth.owner.info.decls)
        Stream(newTermName(imeth.name+"$extension"))
    }
  }

  private def companionModuleForce(sym: Symbol) = {
    sym.andAlso(_.owner.initialize) // See SI-6976. `companionModule` only calls `rawInfo`. (Why?)
    sym.companionModule
  }

  /** Return the extension method that corresponds to given instance method `meth`. */
  def extensionMethod(imeth: Symbol): Symbol = enteringPhase(currentRun.refchecksPhase) {
    val companionInfo = companionModuleForce(imeth.owner).info
    val candidates = extensionNames(imeth) map (companionInfo.decl(_)) filter (_.exists)
    val matching = candidates filter (alt => normalize(alt.tpe, imeth.owner) matches imeth.tpe)
    assert(matching.nonEmpty,
      sm"""|no extension method found for:
           |
           |  $imeth:${imeth.tpe}
           |
           | Candidates:
           |
           | ${candidates.map(c => c.name+":"+c.tpe).mkString("\n")}
           |
           | Candidates (signatures normalized):
           |
           | ${candidates.map(c => c.name+":"+normalize(c.tpe, imeth.owner)).mkString("\n")}
           |
           | Eligible Names: ${extensionNames(imeth).mkString(",")}" """)
    matching.head
  }

  /** Recognize a MethodType which represents an extension method.
   *
   *  It may have a curried parameter list with the `$this` alone in the first
   *  parameter list, in which case that parameter list is dropped.  Or, since
   *  the curried lists disappear during uncurry, it may have a single parameter
   *  list with `$this` as the first parameter, in which case that parameter is
   *  removed from the list.
   */
  object ExtensionMethodType {
    def unapply(tp: Type) = tp match {
      case MethodType(thiz :: rest, restpe) if thiz.name == nme.SELF =>
        Some((thiz, if (rest.isEmpty) restpe else MethodType(rest, restpe) ))
      case _ =>
        None
    }
  }

  /** This method removes the `$this` argument from the parameter list a method.
   *
   *  A method may be a `PolyType`, in which case we tear out the `$this` and the class
   *  type params from its nested `MethodType`.  Or it may be a MethodType, as
   *  described at the ExtensionMethodType extractor.
   */
  private def normalize(stpe: Type, clazz: Symbol): Type = stpe match {
    case PolyType(tparams, restpe) =>
      // method type parameters, class type parameters
      val (mtparams, ctparams) = tparams splitAt (tparams.length - clazz.typeParams.length)
      GenPolyType(mtparams,
        normalize(restpe.substSym(ctparams, clazz.typeParams), clazz))
    case ExtensionMethodType(thiz, etpe) =>
      etpe.substituteTypes(thiz :: Nil, clazz.thisType :: Nil)
    case _ =>
      stpe
  }

  class Extender(unit: CompilationUnit) extends TypingTransformer(unit) {
    private val extensionDefs = mutable.Map[Symbol, mutable.ListBuffer[Tree]]()

    def checkNonCyclic(pos: Position, seen: Set[Symbol], clazz: Symbol): Unit =
      if (seen contains clazz)
        reporter.error(pos, "value class may not unbox to itself")
      else {
        val unboxed = definitions.underlyingOfValueClass(clazz).typeSymbol
        if (unboxed.isDerivedValueClass) checkNonCyclic(pos, seen + clazz, unboxed)
      }

   /** We will need to clone the info of the original method (which obtains clones
    *  of the method type parameters), clone the type parameters of the value class,
    *  and create a new polymethod with the union of all those type parameters, with
    *  their infos adjusted to be consistent with their new home. Example:
    *
    *    class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
    *      def baz[B >: A](x: B): List[B] = x :: xs
    *      // baz has to be transformed into this extension method, where
    *      // A is cloned from class Foo and  B is cloned from method baz:
    *      // def extension$baz[B >: A <: Any, A >: Nothing <: AnyRef]($this: Foo[A])(x: B): List[B]
    *    }
    *
    *  TODO: factor out the logic for consolidating type parameters from a class
    *  and a method for re-use elsewhere, because nobody will get this right without
    *  some higher level facilities.
    */
    def extensionMethInfo(extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type = {
      val GenPolyType(tparamsFromMethod, methodResult) = origInfo cloneInfo extensionMeth
      // Start with the class type parameters - clones will be method type parameters
      // so must drop their variance.
      val tparamsFromClass = cloneSymbolsAtOwner(clazz.typeParams, extensionMeth) map (_ resetFlag COVARIANT | CONTRAVARIANT)

      val thisParamType = appliedType(clazz, tparamsFromClass map (_.tpeHK): _*)
      val thisParam     = extensionMeth.newValueParameter(nme.SELF, extensionMeth.pos) setInfo thisParamType
      val resultType    = MethodType(List(thisParam), dropNullaryMethod(methodResult))
      val selfParamType = singleType(currentOwner.companionModule.thisType, thisParam)

      def fixres(tp: Type)    = tp substThisAndSym (clazz, selfParamType, clazz.typeParams, tparamsFromClass)
      def fixtparam(tp: Type) = tp substSym (clazz.typeParams, tparamsFromClass)

      // We can't substitute symbols on the entire polytype because we
      // need to modify the bounds of the cloned type parameters, but we
      // don't want to substitute for the cloned type parameters themselves.
      val tparams = tparamsFromMethod ::: tparamsFromClass
      GenPolyType(tparams map (_ modifyInfo fixtparam), fixres(resultType))

      // For reference, calling fix on the GenPolyType plays out like this:
      // error: scala.reflect.internal.Types$TypeError: type arguments [B#7344,A#6966]
      // do not conform to method extension$baz#16148's type parameter bounds
      //
      // And the difference is visible here.  See how B is bounded from below by A#16149
      // in both cases, but in the failing case, the other type parameter has turned into
      // a different A. (What is that A? It is a clone of the original A created in
      // SubstMap during the call to substSym, but I am not clear on all the particulars.)
      //
      //  bad: [B#16154 >: A#16149, A#16155 <: AnyRef#2189]($this#16156: Foo#6965[A#16155])(x#16157: B#16154)List#2457[B#16154]
      // good: [B#16151 >: A#16149, A#16149 <: AnyRef#2189]($this#16150: Foo#6965[A#16149])(x#16153: B#16151)List#2457[B#16151]
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case Template(_, _, _) =>
          if (currentOwner.isDerivedValueClass) {
          /* This is currently redundant since value classes may not
             wrap over other value classes anyway.
            checkNonCyclic(currentOwner.pos, Set(), currentOwner) */
            extensionDefs(currentOwner.companionModule) = new mutable.ListBuffer[Tree]
            currentOwner.primaryConstructor.makeNotPrivate(NoSymbol)
            // SI-7859 make param accessors accessible so the erasure can generate unbox operations.
            currentOwner.info.decls.foreach(sym => if (sym.isParamAccessor && sym.isMethod) sym.makeNotPrivate(currentOwner))
            super.transform(tree)
          } else if (currentOwner.isStaticOwner) {
            super.transform(tree)
          } else tree
        case DefDef(_, _, tparams, vparamss, _, rhs) if tree.symbol.isMethodWithExtension =>
          val origMeth      = tree.symbol
          val origThis      = currentOwner
          val origTpeParams = tparams.map(_.symbol) ::: origThis.typeParams   // method type params ++ class type params
          val origParams    = vparamss.flatten map (_.symbol)
          val companion     = origThis.companionModule

          def makeExtensionMethodSymbol = {
            val extensionName = extensionNames(origMeth).head.toTermName
            val extensionMeth = (
              companion.moduleClass.newMethod(extensionName, tree.pos.focus, origMeth.flags & ~OVERRIDE & ~PROTECTED & ~PRIVATE & ~LOCAL | FINAL)
                setAnnotations origMeth.annotations
            )
            origMeth.removeAnnotation(TailrecClass) // it's on the extension method, now.
            companion.info.decls.enter(extensionMeth)
          }

          val extensionMeth = makeExtensionMethodSymbol
          val newInfo       = extensionMethInfo(extensionMeth, origMeth.info, origThis)
          extensionMeth setInfo newInfo

          log(s"Value class $origThis spawns extension method.\n  Old: ${origMeth.defString}\n  New: ${extensionMeth.defString}")

          val GenPolyType(extensionTpeParams, MethodType(thiz :: Nil, extensionMono)) = newInfo
          val extensionParams = allParameters(extensionMono)
          val extensionThis   = gen.mkAttributedStableRef(thiz setPos extensionMeth.pos)

          val extensionBody: Tree = {
            val tree = rhs
              .substituteSymbols(origTpeParams, extensionTpeParams)
              .substituteSymbols(origParams, extensionParams)
              .substituteThis(origThis, extensionThis)
              .changeOwner(origMeth -> extensionMeth)
            new SubstututeRecursion(origMeth, extensionMeth, unit).transform(tree)
          }
          val castBody =
            if (extensionBody.tpe <:< extensionMono.finalResultType)
              extensionBody
            else
              gen.mkCastPreservingAnnotations(extensionBody, extensionMono.finalResultType) // SI-7818 e.g. mismatched existential skolems

          // Record the extension method. Later, in `Extender#transformStats`, these will be added to the companion object.
          extensionDefs(companion) += DefDef(extensionMeth, castBody)

          // These three lines are assembling Foo.bar$extension[T1, T2, ...]($this)
          // which leaves the actual argument application for extensionCall.
          // SI-9542 We form the selection here from the thisType of the companion's owner. This is motivated
          //         by the test case, and is a valid way to construct the reference because we know that this
          //         method is also enclosed by that owner.
          val sel        = Select(gen.mkAttributedRef(companion.owner.thisType, companion), extensionMeth)
          val targs      = origTpeParams map (_.tpeHK)
          val callPrefix = gen.mkMethodCall(sel, targs, This(origThis) :: Nil)

          // Apply all the argument lists.
          deriveDefDef(tree)(_ =>
            atOwner(origMeth)(
              localTyper.typedPos(rhs.pos)(
                gen.mkForwarder(callPrefix, mmap(vparamss)(_.symbol))
              )
            )
          )
        case _ =>
          super.transform(tree)
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      super.transformStats(stats, exprOwner) map {
        case md @ ModuleDef(_, _, _) =>
          val extraStats = extensionDefs remove md.symbol match {
            case Some(defns) => defns.toList map (defn => atOwner(md.symbol)(localTyper.typedPos(md.pos.focus)(defn.duplicate)))
            case _           => Nil
          }
          if (extraStats.isEmpty) md
          else deriveModuleDef(md)(tmpl => deriveTemplate(tmpl)(_ ++ extraStats))
        case stat =>
          stat
      }
  }

  final class SubstututeRecursion(origMeth: Symbol, extensionMeth: Symbol,
                            unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree match {
      // SI-6574 Rewrite recursive calls against the extension method so they can
      //         be tail call optimized later. The tailcalls phases comes before
      //         erasure, which performs this translation more generally at all call
      //         sites.
      //
      //         // Source
      //         class C[C] { def meth[M](a: A) = { { <expr>: C[C'] }.meth[M'] } }
      //
      //         // Translation
      //         class C[C] { def meth[M](a: A) = { { <expr>: C[C'] }.meth[M'](a1) } }
      //         object C   { def meth$extension[M, C](this$: C[C], a: A)
      //                        = { meth$extension[M', C']({ <expr>: C[C'] })(a1) } }
      case treeInfo.Applied(sel @ Select(qual, _), targs, argss) if sel.symbol == origMeth =>
        localTyper.typedPos(tree.pos) {
          val allArgss = List(qual) :: argss
          val origThis = extensionMeth.owner.companionClass
          val baseType = qual.tpe.baseType(origThis)
          val allTargs = targs.map(_.tpe) ::: baseType.typeArgs
          val fun = gen.mkAttributedTypeApply(gen.mkAttributedThis(extensionMeth.owner), extensionMeth, allTargs)
          allArgss.foldLeft(fun)(Apply(_, _))
        }
      case _ => super.transform(tree)
    }
  }
}
