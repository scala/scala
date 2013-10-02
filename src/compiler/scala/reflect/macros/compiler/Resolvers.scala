package scala.reflect.macros
package compiler

import scala.reflect.internal.Flags._
import scala.reflect.macros.TypecheckException

trait Resolvers {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import definitions.{EmptyPackageClass => _, _}
  import treeInfo._
  import gen._

  /** Determines the type of context implied by the macro def.
   */
  val ctxTpe = MacroContextClass.tpe

  /** Resolves a macro impl reference provided in the right-hand side of the given macro definition.
   *
   *  Acceptable shapes of the right-hand side:
   *    1) [<static object>].<method name>[[<type args>]] // vanilla macro def
   *    2) [<macro bundle>].<method name>[[<type args>]]  // shiny new macro bundle
   *
   *  Produces a tree, which represents a reference to a macro implementation if everything goes well,
   *  otherwise reports found errors and returns EmptyTree. The resulting tree should have the following format:
   *
   *    qualifier.method[targs]
   *
   *  Qualifier here might be omitted (local macro defs), be a static object (vanilla macro defs)
   *  or be a dummy instance of a macro bundle (e.g. new MyMacro(???).expand).
   */
  lazy val macroImplRef: Tree = {
    val (maybeBundleRef, methName, targs) = macroDdef.rhs match {
      case Applied(Select(Applied(RefTree(qual, bundleName), _, Nil), methName), targs, Nil) =>
        (RefTree(qual, bundleName.toTypeName), methName, targs)
      case Applied(Ident(methName), targs, Nil) =>
        (Ident(context.owner.enclClass), methName, targs)
      case _ =>
        (EmptyTree, TermName(""), Nil)
    }

    val untypedImplRef = typer.silent(_.typedTypeConstructor(maybeBundleRef)) match {
      case SilentResultValue(result) if result.tpe.baseClasses.contains(MacroClass) =>
        val bundleProto = result.tpe.typeSymbol
        val bundlePkg = bundleProto.enclosingPackageClass
        if (!isMacroBundleProtoType(bundleProto.tpe)) MacroBundleWrongShapeError()
        if (!bundleProto.owner.isStaticOwner) MacroBundleNonStaticError()

        // synthesize the bundle, i.e. given a static `trait Foo extends Macro { def expand = ... } `
        // create a top-level definition `class Foo$Bundle(val c: Context) extends Foo` in a package next to `Foo`
        val bundlePid = gen.mkUnattributedRef(bundlePkg)
        val bundlePrefix =
          if (bundlePkg == EmptyPackageClass) bundleProto.fullName('$')
          else bundleProto.fullName('$').substring(bundlePkg.fullName('$').length + 1)
        val bundleName = TypeName(bundlePrefix + tpnme.MACRO_BUNDLE_SUFFIX)
        val existingBundle = bundleProto.enclosingPackageClass.info.decl(bundleName)
        if (!currentRun.compiles(existingBundle)) {
          def mkContextValDef(flags: Long) = ValDef(Modifiers(flags), nme.c, TypeTree(ctxTpe), EmptyTree)
          val contextField = mkContextValDef(PARAMACCESSOR)
          val contextParam = mkContextValDef(PARAM | PARAMACCESSOR)
          val bundleCtor = DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, List(List(contextParam)), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
          val bundleParent = gen.mkAppliedTypeTree(Ident(bundleProto), bundleProto.typeParams.map(sym => Ident(sym.name)))
          val bundleTemplate = Template(List(bundleParent), noSelfType, List(contextField, bundleCtor))
          val bundle = atPos(bundleProto.pos)(ClassDef(NoMods, bundleName, bundleProto.typeParams.map(TypeDef(_)), bundleTemplate))
          currentRun.compileLate(bundleName + ".scala", PackageDef(bundlePid, List(bundle)))
        }

        // synthesize the macro impl reference, which is going to look like:
        // `new FooBundle(???).macroName` plus the optional type arguments
        val bundleInstance = New(Select(bundlePid, bundleName), List(List(Ident(Predef_???))))
        atPos(macroDdef.rhs.pos)(gen.mkTypeApply(Select(bundleInstance, methName), targs))
      case _ =>
        macroDdef.rhs
    }

    val typedImplRef = typer.silent(_.typed(markMacroImplRef(untypedImplRef)), reportAmbiguousErrors = false)
    typedImplRef match {
      case SilentResultValue(success) => success
      case SilentTypeError(err) => abort(err.errPos, err.errMsg)
    }
  }

  // FIXME: cannot write this concisely because of SI-7507
  // lazy val (isImplBundle, macroImplOwner, macroImpl, macroImplTargs) =
  private lazy val dissectedMacroImplRef =
    macroImplRef match {
      case MacroImplReference(isBundle, owner, meth, targs) => (isBundle, owner, meth, targs)
      case _ => MacroImplReferenceWrongShapeError()
    }
  lazy val isImplBundle = dissectedMacroImplRef._1
  lazy val isImplMethod = !isImplBundle
  lazy val macroImplOwner = dissectedMacroImplRef._2
  lazy val macroImpl = dissectedMacroImplRef._3
  lazy val targs = dissectedMacroImplRef._4
}
