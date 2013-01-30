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
           | Eligible Names: ${extensionNames(imeth).mkString(",")}"""")
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
      // Split the type parameters of the extension method into two groups,
      // corresponding the to class and method type parameters.
      val numClassParams = clazz.typeParams.length
      val methTParams = tparams dropRight numClassParams
      val classTParams = tparams takeRight numClassParams

      GenPolyType(methTParams,
        normalize(restpe.substSym(classTParams, clazz.typeParams), clazz))
    case MethodType(List(thiz), restpe) if thiz.name == nme.SELF =>
      restpe.substituteTypes(thiz :: Nil, clazz.thisType :: Nil)
    case MethodType(thiz :: params, restpe) =>
      MethodType(params, restpe)
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

    def extensionMethInfo(extensionMeth: Symbol, origInfo: Type, clazz: Symbol): Type = {
      // No variance for method type parameters
      var newTypeParams = cloneSymbolsAtOwner(clazz.typeParams, extensionMeth) map (_ resetFlag COVARIANT | CONTRAVARIANT)
      val thisParamType = appliedType(clazz.typeConstructor, newTypeParams map (_.tpeHK))
      val thisParam     = extensionMeth.newValueParameter(nme.SELF, extensionMeth.pos) setInfo thisParamType
      def transform(clonedType: Type): Type = clonedType match {
        case MethodType(params, restpe) =>
          // I assume it was a bug that this was dropping params... [Martin]: No, it wasn't; it's curried.
          MethodType(List(thisParam), clonedType)
        case NullaryMethodType(restpe) =>
          MethodType(List(thisParam), restpe)
      }
      val GenPolyType(tparams, restpe) = origInfo cloneInfo extensionMeth
      val selfParamSingletonType = singleType(currentOwner.companionModule.thisType, thisParam)
      GenPolyType(
        tparams ::: newTypeParams,
        transform(restpe) substThisAndSym (clazz, selfParamSingletonType, clazz.typeParams, newTypeParams)
      )
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

          def thisParamRef = gen.mkAttributedStableRef(extensionMeth.info.params.head setPos extensionMeth.pos)
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
