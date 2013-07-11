package scala.reflect.macros
package compiler

import scala.reflect.internal.Flags._
import scala.reflect.macros.TypecheckException

trait Resolvers {
  self: DefaultMacroCompiler =>

  import global._
  import analyzer._
  import definitions._
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
      case Applied(methRef @ Select(bundleRef @ RefTree(qual, bundleName), methName), targs, Nil) =>
        (RefTree(qual, bundleName.toTypeName), methName, targs)
      case Applied(Ident(methName), targs, Nil) =>
        (Ident(context.owner.enclClass), methName, targs)
      case _ =>
        (EmptyTree, TermName(""), Nil)
    }

    val untypedImplRef = typer.silent(_.typedType(maybeBundleRef)) match {
      case SilentResultValue(result) if isMacroBundleType(result.tpe) =>
        val bundleClass = result.tpe.typeSymbol
        if (!bundleClass.owner.isPackageClass) abort(macroDef.pos, "macro bundles can only be defined as top-level classes or traits")

        // synthesize the invoker, i.e. given a top-level `trait Foo extends Macro { def expand = ... } `
        // create a top-level definition `class FooInvoker(val c: Context) extends Foo` in MACRO_INVOKER_PACKAGE
        val invokerPid = gen.mkUnattributedRef(nme.MACRO_INVOKER_PACKAGE)
        val invokerName = TypeName(bundleClass.fullName.split('.').map(_.capitalize).mkString("") + nme.MACRO_INVOKER_SUFFIX)
        val invokerFullName = TypeName(s"$invokerPid.$invokerName")
        val existingInvoker = rootMirror.getClassIfDefined(invokerFullName)
        if (!currentRun.compiles(existingInvoker)) {
          def mkContextValDef(flags: Long) = ValDef(Modifiers(flags), nme.c, TypeTree(ctxTpe), EmptyTree)
          val contextField = mkContextValDef(PARAMACCESSOR)
          val contextParam = mkContextValDef(PARAM | PARAMACCESSOR)
          val invokerCtor = DefDef(Modifiers(), nme.CONSTRUCTOR, Nil, List(List(contextParam)), TypeTree(), Block(List(pendingSuperCall), Literal(Constant(()))))
          val invoker = atPos(bundleClass.pos)(ClassDef(NoMods, invokerName, Nil, Template(List(Ident(bundleClass)), emptyValDef, List(contextField, invokerCtor))))
          currentRun.compileLate(PackageDef(invokerPid, List(invoker)))
        }

        // synthesize the macro impl reference, which is going to look like:
        // `new Foo$invoker(???).expand` plus the optional type arguments
        val instanceOfInvoker = New(Select(invokerPid, invokerName), List(List(Select(scalaDot(nme.Predef), nme.???))))
        gen.mkTypeApply(Select(instanceOfInvoker, methName), targs)
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
