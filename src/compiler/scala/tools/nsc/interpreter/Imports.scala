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

  def isNoImports = settings.noimports.value
  def isNoPredef  = settings.nopredef.value

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
  def languageWildcards: List[Type] = languageWildcardSyms map (_.tpe)
  def languageWildcardHandlers = languageWildcardSyms map makeWildcardImportHandler

  def allImportedNames = importHandlers flatMap (_.importedNames)
  def importedTerms    = onlyTerms(allImportedNames)
  def importedTypes    = onlyTypes(allImportedNames)

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
  def wildcardTypes = languageWildcards ++ sessionWildcards

  def languageSymbols        = languageWildcardSyms flatMap membersAtPickler
  def sessionImportedSymbols = importHandlers flatMap (_.importedSymbols)
  def importedSymbols        = languageSymbols ++ sessionImportedSymbols
  def importedTermSymbols    = importedSymbols collect { case x: TermSymbol => x }
  def importedTypeSymbols    = importedSymbols collect { case x: TypeSymbol => x }
  def implicitSymbols        = importedSymbols filter (_.isImplicit)

  def importedTermNamed(name: String): Symbol =
    importedTermSymbols find (_.name.toString == name) getOrElse NoSymbol

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
   *  three pieces of related code:
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
  case class ComputedImports(prepend: String, append: String, access: String)
  protected def importsCode(wanted0: Set[Name]): ComputedImports = {
    val wanted = wanted0 filterNot isUnlinked

    /** Narrow down the list of requests from which imports
     *  should be taken.  Removes requests which cannot contribute
     *  useful imports for the specified set of wanted names.
     */
    case class ReqAndHandler(req: Request, handler: MemberHandler) { }

    def reqsToUse: List[ReqAndHandler] = {
      /** Loop through a list of MemberHandlers and select which ones to keep.
        * 'wanted' is the set of names that need to be imported.
       */
      def select(reqs: List[ReqAndHandler], wanted: Set[Name]): List[ReqAndHandler] = {
        // Single symbol imports might be implicits! See bug #1752.  Rather than
        // try to finesse this, we will mimic all imports for now.
        def keepHandler(handler: MemberHandler) = handler match {
          case _: ImportHandler => true
          case x                => x.definesImplicit || (x.definedNames exists wanted)
        }

        reqs match {
          case Nil                                    => Nil
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

    val code, trailingBraces, accessPath = new StringBuilder
    val currentImps = mutable.HashSet[Name]()

    // add code for a new object to hold some imports
    def addWrapper() {
      val impname = nme.INTERPRETER_IMPORT_WRAPPER
      code append "object %s {\n".format(impname)
      trailingBraces append "}\n"
      accessPath append ("." + impname)
      currentImps.clear()
    }
    def maybeWrap(names: Name*) = if (names exists currentImps) addWrapper()
    def wrapBeforeAndAfter[T](op: => T): T = {
      addWrapper()
      try op finally addWrapper()
    }

    // loop through previous requests, adding imports for each one
    wrapBeforeAndAfter {
      for (ReqAndHandler(req, handler) <- reqsToUse) {
        handler match {
          // If the user entered an import, then just use it; add an import wrapping
          // level if the import might conflict with some other import
          case x: ImportHandler if x.importsWildcard =>
            wrapBeforeAndAfter(code append (x.member + "\n"))
          case x: ImportHandler =>
            maybeWrap(x.importedNames: _*)
            code append (x.member + "\n")
            currentImps ++= x.importedNames

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

    ComputedImports(code.toString, trailingBraces.toString, accessPath.toString)
  }

  private def allReqAndHandlers =
    prevRequestList flatMap (req => req.handlers map (req -> _))

  private def membersAtPickler(sym: Symbol): List[Symbol] =
    enteringPickler(sym.info.nonPrivateMembers.toList)
}
