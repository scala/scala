/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }

trait Imports {
  self: IMain =>

  import global._
  import definitions.{ ObjectClass, ScalaPackage, JavaLangPackage, PredefModule }
  import memberHandlers._

  /** Synthetic import handlers for the language defined imports. */
  private def makeWildcardImportHandler(sym: Symbol): ImportHandler = {
    val hd :: tl = sym.fullName.split('.').toList map newTermName
    val tree = Import(
      tl.foldLeft(Ident(hd): Tree)((x, y) => Select(x, y)),
      ImportSelector.wildList
    )
    tree setSymbol sym
    new ImportHandler(tree)
  }

  /** Symbols whose contents are language-defined to be imported. */
  def languageWildcardSyms: List[Symbol] = List(JavaLangPackage, ScalaPackage, PredefModule)
  def languageWildcardHandlers = languageWildcardSyms map makeWildcardImportHandler

  def allImportedNames = importHandlers flatMap (_.importedNames)

  /** Types which have been wildcard imported, such as:
   *    val x = "abc" ; import x._  // type java.lang.String
   *    import java.lang.String._   // object java.lang.String
   *
   *  Used by tab completion.
   *
   *  XXX right now this gets import x._ and import java.lang.String._,
   *  but doesn't figure out import String._.  There's a lot of ad hoc
   *  scope twiddling which should be swept away in favor of digging
   *  into the compiler scopes.
   */
  def sessionWildcards: List[Type] = {
    importHandlers filter (_.importsWildcard) map (_.targetType) distinct
  }

  def languageSymbols        = languageWildcardSyms flatMap membersAtPickler
  def sessionImportedSymbols = importHandlers flatMap (_.importedSymbols)
  def importedSymbols        = languageSymbols ++ sessionImportedSymbols
  def importedTermSymbols    = importedSymbols collect { case x: TermSymbol => x }

  /** Tuples of (source, imported symbols) in the order they were imported.
   */
  def importedSymbolsBySource: List[(Symbol, List[Symbol])] = {
    val lang    = languageWildcardSyms map (sym => (sym, membersAtPickler(sym)))
    val session = importHandlers filter (_.targetType != NoType) map { mh =>
      (mh.targetType.typeSymbol, mh.importedSymbols)
    }

    lang ++ session
  }
  def implicitSymbolsBySource: List[(Symbol, List[Symbol])] = {
    importedSymbolsBySource map {
      case (k, vs) => (k, vs filter (_.isImplicit))
    } filterNot (_._2.isEmpty)
  }

  /** Compute imports that allow definitions from previous
   *  requests to be visible in a new request.  Returns
   *  three or four pieces of related code:
   *
   *  0. Header code fragment that should go at the beginning
   *  of the compilation unit, specifically, import Predef.
   *
   *  1. An initial code fragment that should go before
   *  the code of the new request.
   *
   *  2. A code fragment that should go after the code
   *  of the new request.
   *
   *  3. An access path which can be traversed to access
   *  any bindings inside code wrapped by #1 and #2 .
   *
   * The argument is a set of Names that need to be imported.
   *
   * Limitations: This method is not as precise as it could be.
   * (1) It does not process wildcard imports to see what exactly
   * they import.
   * (2) If it imports any names from a request, it imports all
   * of them, which is not really necessary.
   * (3) It imports multiple same-named implicits, but only the
   * last one imported is actually usable.
   */
  case class ComputedImports(header: String, prepend: String, append: String, access: String)

  protected def importsCode(wanted: Set[Name], wrapper: Request#Wrapper, definesClass: Boolean, generousImports: Boolean): ComputedImports = {
    val header, code, trailingBraces, accessPath = new StringBuilder
    val currentImps = mutable.HashSet[Name]()
    var predefEscapes = false      // only emit predef import header if name not resolved in history, loosely

    /** Narrow down the list of requests from which imports
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    case class ReqAndHandler(req: Request, handler: MemberHandler)

    def reqsToUse: List[ReqAndHandler] = {
      /** Loop through a list of MemberHandlers and select which ones to keep.
       *  'wanted' is the set of names that need to be imported.
       */
      def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler] = {
        // Single symbol imports might be implicits! See bug #1752.  Rather than
        // try to finesse this, we will mimic all imports for now.
        def keepHandler(handler: MemberHandler) = handler match {
          // While defining classes in class based mode - implicits are not needed.
          case h: ImportHandler if isClassBased && definesClass => h.importedNames.exists(x => wanted.contains(x))
          case _: ImportHandler     => true
          case x if generousImports => x.definesImplicit || (x.definedNames exists (d => wanted.exists(w => d.startsWith(w))))
          case x                    => x.definesImplicit || (x.definedNames exists wanted)
        }

        reqs match {
          case Nil                                    => predefEscapes = wanted contains PredefModule.name ; Nil
          case rh :: rest if !keepHandler(rh.handler) => select(rest, wanted)
          case rh :: rest                             =>
            import rh.handler._
            val newWanted = wanted ++ referencedNames -- definedNames -- importedNames
            rh :: select(rest, newWanted)
        }
      }

      /** Flatten the handlers out and pair each with the original request */
      select(allReqAndHandlers reverseMap { case (r, h) => ReqAndHandler(r, h) }, wanted).reverse
    }

    // add code for a new object to hold some imports
    def addWrapper() {
      import nme.{ INTERPRETER_IMPORT_WRAPPER => iw }
      code append (wrapper.prewrap format iw)
      trailingBraces append wrapper.postwrap
      accessPath append s".$iw"
      currentImps.clear()
    }

    def maybeWrap(names: Name*) = if (names exists currentImps) addWrapper()

    def wrapBeforeAndAfter[T](op: => T): T = {
      addWrapper()
      try op finally addWrapper()
    }

    // imports from Predef are relocated to the template header to allow hiding.
    def checkHeader(h: ImportHandler) = h.referencedNames contains PredefModule.name

    // loop through previous requests, adding imports for each one
    wrapBeforeAndAfter {
      // Reusing a single temporary value when import from a line with multiple definitions.
      val tempValLines = mutable.Set[Int]()
      for (ReqAndHandler(req, handler) <- reqsToUse) {
        val objName = req.lineRep.readPathInstance
        handler match {
          case h: ImportHandler if checkHeader(h) =>
            header.clear()
            header append f"${h.member}%n"
          // If the user entered an import, then just use it; add an import wrapping
          // level if the import might conflict with some other import
          case x: ImportHandler if x.importsWildcard =>
            wrapBeforeAndAfter(code append (x.member + "\n"))
          case x: ImportHandler =>
            maybeWrap(x.importedNames: _*)
            code append (x.member + "\n")
            currentImps ++= x.importedNames

          case x if isClassBased =>
            for (imv <- x.definedNames) {
              if (!currentImps.contains(imv)) {
                x match {
                  case _: ClassHandler =>
                    code.append("import " + objName + req.accessPath + ".`" + imv + "`\n")
                  case _ =>
                    val valName = req.lineRep.packageName + req.lineRep.readName
                    if (!tempValLines.contains(req.lineRep.lineId)) {
                      code.append(s"val $valName: ${objName}.type = $objName\n")
                      tempValLines += req.lineRep.lineId
                    }
                    code.append(s"import $valName${req.accessPath}.`$imv`;\n")
                }
                currentImps += imv
              }
            }
          // For other requests, import each defined name.
          // import them explicitly instead of with _, so that
          // ambiguity errors will not be generated. Also, quote
          // the name of the variable, so that we don't need to
          // handle quoting keywords separately.
          case x =>
            for (sym <- x.definedSymbols) {
              maybeWrap(sym.name)
              code append s"import ${x.path}\n"
              currentImps += sym.name
            }
        }
      }
    }

    val computedHeader = if (predefEscapes) header.toString else ""
    ComputedImports(computedHeader, code.toString, trailingBraces.toString, accessPath.toString)
  }

  private def allReqAndHandlers =
    prevRequestList flatMap (req => req.handlers map (req -> _))

  private def membersAtPickler(sym: Symbol): List[Symbol] =
    enteringPickler(sym.info.nonPrivateMembers.toList)
}
