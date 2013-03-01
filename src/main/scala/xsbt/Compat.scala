package xsbt

import scala.tools.nsc.Global
import scala.tools.nsc.symtab.Flags

/**
 *  Collection of hacks that make it possible for the compiler interface
 *  to stay source compatible with Scala compiler 2.9, 2.10 and 2.11.
 */
abstract class Compat
{
	val global: Global
	import global._
	val LocalChild = global.tpnme.LOCAL_CHILD
	val Nullary = global.NullaryMethodType
	val ScalaObjectClass = definitions.ScalaObjectClass

	private[this] final class MiscCompat
	{
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

	// before 2.10, sym.moduleSuffix doesn't exist, but genJVM.moduleSuffix does
	private[this] implicit def symbolCompat(sym: Symbol): SymbolCompat = new SymbolCompat(sym)
	private[this] final class SymbolCompat(sym: Symbol) {
		def moduleSuffix = global.genJVM.moduleSuffix(sym)
	}


	val DummyValue = 0
	def hasMacro(s: Symbol): Boolean =
	{
		val MACRO = Flags.MACRO // will be DummyValue for versions before 2.10
		MACRO != DummyValue && s.hasFlag(MACRO)
	}
	def moduleSuffix(s: Symbol): String = s.moduleSuffix

	private[this] def sourceCompatibilityOnly: Nothing = throw new RuntimeException("For source compatibility only: should not get here.")

	private[this] final implicit def miscCompat(n: AnyRef): MiscCompat = new MiscCompat
}
