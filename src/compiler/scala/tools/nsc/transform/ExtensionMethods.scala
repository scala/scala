/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package scala.tools.nsc
package transform

import symtab._
import Flags._
import scala.collection.{ mutable, immutable }
import scala.collection.mutable
import scala.tools.nsc.util.FreshNameCreator
import scala.runtime.ScalaRunTime.{ isAnyVal, isTuple }

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
  import typer.{ typed, atOwner } // methods to type trees

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
  def extensionMethod(imeth: Symbol): Symbol = atPhase(currentRun.refchecksPhase) {
    val companionInfo = companionModuleForce(imeth.owner).info
    val candidates = extensionNames(imeth) map (companionInfo.decl(_)) filter (_.exists)
    val matching = candidates filter (alt => normalize(alt.tpe, imeth.owner) matches imeth.tpe)
    assert(matching.nonEmpty,
      s"no extension method found for $imeth:${imeth.tpe} among ${candidates.map(c => c.name+":"+c.tpe).toList} / ${extensionNames(imeth).toList}")
    matching.head
  }

  /** This method removes the `$this` argument from the parameter list a method.
   *
   *  A method may be a `PolyType`, in which case we tear out the `$this` and the class
   *  type params from its nested `MethodType`.
   *  It may be a `MethodType`, either with a curried parameter list in which the first argument
   *  is a `$this` - we just return the rest of the list.
   *  This means that the corresponding symbol was generated during `extmethods`.
   *
   *  It may also be a `MethodType` in which the `$this` does not appear in a curried parameter list.
   *  The curried lists disappear during `uncurry`, and the methods may be duplicated afterwards,
   *  for instance, during `specialize`.
   *  In this case, the first argument is `$this` and we just get rid of it.
   */
  private def normalize(stpe: Type, clazz: Symbol): Type = stpe match {
    case PolyType(tparams, restpe) =>
      GenPolyType(tparams dropRight clazz.typeParams.length, normalize(restpe.substSym(tparams takeRight clazz.typeParams.length, clazz.typeParams), clazz))
    case MethodType(List(thiz), restpe) if thiz.name == nme.SELF =>
      restpe
    case MethodType(tparams, restpe) =>
      MethodType(tparams.drop(1), restpe)
    case _ =>
      stpe
  }

  class Extender(unit: CompilationUnit) extends TypingTransformer(unit) {

    private val extensionDefs = mutable.Map[Symbol, mutable.ListBuffer[Tree]]()

    def checkNonCyclic(pos: Position, seen: Set[Symbol], clazz: Symbol): Unit =
      if (seen contains clazz)
        unit.error(pos, "value class may not unbox to itself")
      else {
        val unboxed = erasure.underlyingOfValueClass(clazz).typeSymbol
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
      var tparamsFromClass = cloneSymbolsAtOwner(clazz.typeParams, extensionMeth) map (_ resetFlag COVARIANT | CONTRAVARIANT)
      def fix(tp: Type) = tp.substSym(clazz.typeParams, tparamsFromClass)

      val thisParamType = appliedType(clazz.typeConstructor, tparamsFromClass map (_.tpeHK))
      val thisParam     = extensionMeth.newValueParameter(nme.SELF, extensionMeth.pos) setInfo thisParamType
      val resultType = methodResult match {
        case MethodType(_, _)          => MethodType(List(thisParam), methodResult)
        case NullaryMethodType(restpe) => MethodType(List(thisParam), restpe)
      }
      // We can't substitute symbols on the entire polytype because we
      // need to modify the bounds of the cloned type parameters, but we
      // don't want to substitute for the cloned type parameters themselves.
      val tparams = tparamsFromMethod ::: tparamsFromClass
      GenPolyType(tparams map (_ modifyInfo fix), fix(resultType))

      // For reference, calling fix on the GenPolyType plays out like this:
      // error: scala.reflect.internal.Types$TypeError: type arguments [B#7344,A#6966]
      // do not conform to method extension$baz#16148's type parameter bounds
      //
      // And the difference is visible here.  See how B is bounded from below by A#16149
      // in both cases, but in the failing case, the other type parameter has turned into
      // a different A.
      //
      //  bad: [B#16154 >: A#16149, A#16155 <: AnyRef#2189]($this#16156: Foo#6965[A#16155])(x#16157: B#16154)List#2457[B#16154]
      // good: [B#16151 >: A#16149, A#16149 <: AnyRef#2189]($this#16150: Foo#6965[A#16149])(x#16153: B#16151)List#2457[B#16151]
    }

    private def allParams(tpe: Type): List[Symbol] = tpe match {
      case MethodType(params, res) => params ::: allParams(res)
      case _ => List()
    }

    override def transform(tree: Tree): Tree = {
      tree match {
        case Template(_, _, _) =>
          if (currentOwner.isDerivedValueClass) {
          /* This is currently redundant since value classes may not
             wrap over other value classes anyway.
            checkNonCyclic(currentOwner.pos, Set(), currentOwner) */
            extensionDefs(currentOwner.companionModule) = new mutable.ListBuffer[Tree]
            super.transform(tree)
          } else if (currentOwner.isStaticOwner) {
            super.transform(tree)
          } else tree
        case DefDef(_, _, tparams, vparamss, _, rhs) if tree.symbol.isMethodWithExtension =>
          val companion = currentOwner.companionModule
          val origMeth = tree.symbol
          val extensionName = extensionNames(origMeth).head
          val extensionMeth = companion.moduleClass.newMethod(extensionName, origMeth.pos, origMeth.flags & ~OVERRIDE & ~PROTECTED | FINAL)
            .setAnnotations(origMeth.annotations)
          companion.info.decls.enter(extensionMeth)
          val newInfo = extensionMethInfo(extensionMeth, origMeth.info, currentOwner)
          extensionMeth setInfo newInfo
          log("Value class %s spawns extension method.\n  Old: %s\n  New: %s".format(
            currentOwner,
            origMeth.defString,
            extensionMeth.defString)) // extensionMeth.defStringSeenAs(origInfo

          def thisParamRef = gen.mkAttributedIdent(extensionMeth.info.params.head setPos extensionMeth.pos)
          val GenPolyType(extensionTpeParams, extensionMono) = extensionMeth.info
          val origTpeParams = (tparams map (_.symbol)) ::: currentOwner.typeParams
          val extensionBody = rhs
              .substituteSymbols(origTpeParams, extensionTpeParams)
              .substituteSymbols(vparamss.flatten map (_.symbol), allParams(extensionMono).tail)
              .substituteThis(currentOwner, thisParamRef)
              .changeOwner((origMeth, extensionMeth))
          extensionDefs(companion) += atPos(tree.pos) { DefDef(extensionMeth, extensionBody) }
          val extensionCallPrefix = Apply(
              gen.mkTypeApply(gen.mkAttributedRef(companion), extensionMeth, origTpeParams map (_.tpeHK)),
              List(This(currentOwner)))
          val extensionCall = atOwner(origMeth) {
            localTyper.typedPos(rhs.pos) {
              gen.mkForwarder(extensionCallPrefix, mmap(vparamss)(_.symbol))
            }
          }
          deriveDefDef(tree)(_ => extensionCall)
        case _ =>
          super.transform(tree)
      }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] =
      super.transformStats(stats, exprOwner) map {
        case md @ ModuleDef(_, _, _) if extensionDefs contains md.symbol =>
          val defns = extensionDefs(md.symbol).toList map (member =>
            atOwner(md.symbol)(localTyper.typedPos(md.pos.focus)(member))
          )
          extensionDefs -= md.symbol
          deriveModuleDef(md)(tmpl => deriveTemplate(tmpl)(_ ++ defns))
        case stat =>
          stat
      }
  }
}
