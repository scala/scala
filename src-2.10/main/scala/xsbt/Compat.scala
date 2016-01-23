package xsbt

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags

/**
 * Collection of hacks that make it possible for the compiler interface
 * to stay source compatible with Scala compiler 2.9, 2.10 and 2.11.
 *
 * One common technique used in `Compat` class is use of implicit conversions to deal
 * with methods that got renamed or moved between different Scala compiler versions.
 *
 * Let's pick a specific example. In Scala 2.9 and 2.10 there was a method called `toplevelClass`
 * defined on `Symbol`. In 2.10 that method has been deprecated and `enclosingTopLevelClass`
 * method has been introduce as a replacement. In Scala 2.11 the old `toplevelClass` method has
 * been removed. How can we pick the right version based on availability of those two methods?
 *
 * We define an implicit conversion from Symbol to a class that contains both method definitions:
 *
 *   implicit def symbolCompat(sym: Symbol): SymbolCompat = new SymbolCompat(sym)
 *   class SymbolCompat(sym: Symbol) {
 *     def enclosingTopLevelClass: Symbol = sym.toplevelClass
 *     def toplevelClass: Symbol =
 *       throw new RuntimeException("For source compatibility only: should not get here.")
 *   }
 *
 * We assume that client code (code in compiler interface) should always call `enclosingTopLevelClass`
 * method. If we compile that code against 2.11 it will just directly link against method provided by
 * Symbol. However, if we compile against 2.9 or 2.10 `enclosingTopLevelClass` won't be found so the
 * implicit conversion defined above will kick in. That conversion will provide `enclosingTopLevelClass`
 * that simply forwards to the old `toplevelClass` method that is available in 2.9 and 2.10 so that
 * method will be called in the end. There's one twist: since `enclosingTopLevelClass` forwards to
 * `toplevelClass` which doesn't exist in 2.11! Therefore, we need to also define `toplevelClass`
 * that will be provided by an implicit conversion as well. However, we should never reach that method
 * at runtime if either `enclosingTopLevelClass` or `toplevelClass` is available on Symbol so this
 * is purely source compatibility stub.
 *
 * The technique described above is used in several places below.
 *
 */
abstract class Compat {
  val global: Global
  import global._
  val LocalChild = global.tpnme.LOCAL_CHILD
  val Nullary = global.NullaryMethodType
  val ScalaObjectClass = definitions.ScalaObjectClass

  // `transformedType` doesn't exist in Scala < 2.10
  implicit def withTransformedType(global: Global): WithTransformedType = new WithTransformedType(global)
  class WithTransformedType(global: Global) {
    def transformedType(tpe: Type): Type = tpe
  }

  private[this] final class MiscCompat {
    // in 2.9, nme.LOCALCHILD was renamed to tpnme.LOCAL_CHILD
    def tpnme = nme
    def LOCAL_CHILD = nme.LOCALCHILD
    def LOCALCHILD = sourceCompatibilityOnly

    // in 2.10, ScalaObject was removed
    def ScalaObjectClass = definitions.ObjectClass

    def NullaryMethodType = NullaryMethodTpe

    def MACRO = DummyValue

    // in 2.10, sym.moduleSuffix exists, but genJVM.moduleSuffix(Symbol) does not
    def moduleSuffix(sym: Symbol): String = sourceCompatibilityOnly
    // in 2.11 genJVM does not exist
    def genJVM = this
  }
  // in 2.9, NullaryMethodType was added to Type
  object NullaryMethodTpe {
    def unapply(t: Type): Option[Type] = None
  }

  protected implicit def symbolCompat(sym: Symbol): SymbolCompat = new SymbolCompat(sym)
  protected final class SymbolCompat(sym: Symbol) {
    // before 2.10, sym.moduleSuffix doesn't exist, but genJVM.moduleSuffix does
    def moduleSuffix = global.genJVM.moduleSuffix(sym)

    def enclosingTopLevelClass: Symbol = sym.toplevelClass
    def toplevelClass: Symbol = sourceCompatibilityOnly
    def asMethod: MethodSymbol = sym.asInstanceOf[MethodSymbol]
  }

  val DummyValue = 0
  def hasMacro(s: Symbol): Boolean =
    {
      val MACRO = Flags.MACRO // will be DummyValue for versions before 2.10
      MACRO != DummyValue && s.hasFlag(MACRO.toLong)
    }
  def moduleSuffix(s: Symbol): String = s.moduleSuffix

  private[this] def sourceCompatibilityOnly: Nothing = throw new RuntimeException("For source compatibility only: should not get here.")

  private[this] final implicit def miscCompat(n: AnyRef): MiscCompat = new MiscCompat

  object MirrorHelper {

    private implicit def withRootMirror(x: Any): WithRootMirror = new WithRootMirror(x)
    private class DummyMirror {
      def getClassIfDefined(x: String): Symbol = NoSymbol
    }
    private class WithRootMirror(x: Any) {
      def rootMirror: DummyMirror = new DummyMirror
    }
    lazy val AnyValClass = global.rootMirror.getClassIfDefined("scala.AnyVal")

    def isDerivedValueClass(sym: Symbol): Boolean =
      sym.isNonBottomSubClass(AnyValClass) && !definitions.ScalaValueClasses.contains(sym)
  }

  object MacroExpansionOf {
    def unapply(tree: Tree): Option[Tree] = {

      // MacroExpansionAttachment (MEA) compatibility for 2.8.x and 2.9.x
      object Compat {
        class MacroExpansionAttachment(val original: Tree)

        // Trees have no attachments in 2.8.x and 2.9.x
        implicit def withAttachments(tree: Tree): WithAttachments = new WithAttachments(tree)
        class WithAttachments(val tree: Tree) {
          object EmptyAttachments {
            def all = Set.empty[Any]
          }
          val attachments = EmptyAttachments
        }
      }
      import Compat._

      locally {
        // Wildcard imports are necessary since 2.8.x and 2.9.x don't have `MacroExpansionAttachment` at all
        import global._ // this is where MEA lives in 2.10.x

        // `original` has been renamed to `expandee` in 2.11.x
        implicit def withExpandee(att: MacroExpansionAttachment): WithExpandee = new WithExpandee(att)
        class WithExpandee(att: MacroExpansionAttachment) {
          def expandee: Tree = att.original
        }

        locally {
          import analyzer._ // this is where MEA lives in 2.11.x
          tree.attachments.all.collect {
            case att: MacroExpansionAttachment => att.expandee
          }.headOption
        }
      }
    }
  }
}
