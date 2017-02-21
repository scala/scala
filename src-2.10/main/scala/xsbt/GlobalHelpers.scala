package xsbt

import scala.tools.nsc.Global

trait GlobalHelpers {
  self: Compat =>
  val global: CallbackGlobal
  import global._

  /** Return true if type shall be ignored, false otherwise. */
  @inline def ignoredType(tpe: Type) = {
    tpe == null ||
      tpe == NoType ||
      tpe.typeSymbol == EmptyPackageClass
  }

  /** Return true if symbol shall be ignored, false otherwise. */
  @inline def ignoredSymbol(symbol: Symbol) = {
    symbol == null ||
      symbol == NoSymbol ||
      symbol == EmptyPackageClass
  }

  /** Return true if name is empty, false otherwise. */
  def isEmptyName(name: Name): Boolean = {
    name match {
      case null | nme.EMPTY | nme.EMPTY_PACKAGE_NAME |
        tpnme.EMPTY | tpnme.EMPTY_PACKAGE_NAME => true
      case _ => false
    }
  }

  /** Apply `op` on every type symbol which doesn't represent a package. */
  def foreachNotPackageSymbolInType(tpe: Type)(op: Symbol => Unit): Unit = {
    new ForEachTypeTraverser(_ match {
      case null =>
      case tpe =>
        val sym = tpe.typeSymbolDirect
        if (sym != NoSymbol && !sym.hasPackageFlag) op(sym)
    }).traverse(tpe)
  }

  /** Returns true if given tree contains macro attchment. In such case calls func on tree from attachment. */
  def processMacroExpansion(in: Tree)(func: Tree => Unit): Boolean = {
    // Hotspot
    var seen = false
    in.attachments.all.foreach {
      case _ if seen =>
      case macroAttachment: MacroExpansionAttachment =>
        func(macroAttachment.original)
        seen = true
      case _ =>
    }
    seen
  }

  /** Define common error messages for error reporting and assertions. */
  object Feedback {
    val NameHashingDisabled = "Turning off name hashing is not supported in class-based dependency trackging."
    val OrphanTopLevelImports = noTopLevelMember("top level imports")
    val OrphanNames = noTopLevelMember("names")

    def noOriginFileForExternalSymbol(symbol: Symbol) =
      s"The symbol $symbol comes from an unknown source or compiled source -- ignoring."
    def expectedClassSymbol(culprit: Symbol): String =
      s"The ${culprit.fullName} defined at ${culprit.fullLocationString} is not a class symbol."
    def missingEnclosingClass(culprit: Symbol, owner: Symbol): String =
      s"No enclosing class. Discarding dependency on $culprit (currentOwner = $owner)."
    def noTopLevelMember(found: String) = s"""
      |Found $found but no class, trait or object is defined in the compilation unit.
      |The incremental compiler cannot record the dependency information in such case.
      |Some errors like unused import referring to a non-existent class might not be reported.
    """.stripMargin
  }
}
