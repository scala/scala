/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package interpreter

import scala.collection.mutable

/** A magic symbol that, when imported in a REPL template, bumps the
 *  effective nesting level of the typechecker.
 *
 *  The REPL inserts this import to control scoping in code templates,
 *  without excessive lexical noise.
 *
 *  {{{
 *  import p.X
 *  import scala.tools.nsc.interpreter.`{{`
 *  import q.X
 *  X           // q.X
 *  }}}
 *
 *  Its name is chosen to suggest scoping by braces; the brace is doubled
 *  to avoid confusion in printed output, as the name will be visible to
 *  a REPL user inspecting generated code.
 *
 *  There is no complementary symbol to restore the nesting level.
 */
object `{{`

trait Imports {
  self: IMain =>

  import global.{Tree, Import, ImportSelector, Select, Ident, TermName, Symbol, NoType, Name, enteringPickler}
  import global.nme.{ INTERPRETER_IMPORT_WRAPPER => iw, INTERPRETER_IMPORT_LEVEL_UP }
  import global.definitions.{ ScalaPackage, JavaLangPackage, PredefModule }
  import memberHandlers._

  /** Synthetic import handlers for the language defined imports. */
  private def makeWildcardImportHandler(sym: Symbol): ImportHandler = {
    val hd :: tl = sym.fullName.split('.').toList.map(TermName(_)): @unchecked
    val tree = Import(
      tl.foldLeft(Ident(hd): Tree)((x, y) => Select(x, y)),
      ImportSelector.wildList
    )
    tree setSymbol sym
    new ImportHandler(tree)
  }

  /** Symbols whose contents are language-defined to be imported. */
  private def languageWildcardSyms: List[Symbol] = List(JavaLangPackage, ScalaPackage, PredefModule)
  def languageWildcardHandlers = languageWildcardSyms map makeWildcardImportHandler

  /** Tuples of (source, imported symbols) in the order they were imported.
   */
  private def importedSymbolsBySource: List[(Symbol, List[Symbol])] = {
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

  protected def importsCode(wanted: Set[Name], request: Request, generousImports: Boolean): ComputedImports = {
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
          case _: ImportHandler     => true
          case x if generousImports => x.definesImplicit || (x.definedNames exists (d => wanted.exists(w => d.startsWith(w))))
          case x                    => x.definesImplicit || (x.definedNames exists wanted)
        }

        reqs match {
          case Nil                                    => predefEscapes = wanted contains PredefModule.name ; Nil
          case rh :: rest if !keepHandler(rh.handler) => select(rest, wanted)
          case rh :: rest                             =>
            import rh.handler._
            val augment = rh match {
              case ReqAndHandler(_, _: ImportHandler) => referencedNames            // for "import a.b", add "a" to names to be resolved
              case _ => Nil
            }
            val newWanted = wanted ++ augment diff definedNames.toSet diff importedNames.toSet
            rh :: select(rest, newWanted)
        }
      }

      /** Flatten the handlers out and pair each with the original request */
      select(allReqAndHandlers.reverseIterator.map { case (r, h) => ReqAndHandler(r, h) }.toList, wanted).reverse
    }

    def addLevelChangingImport() = code.append(s"import _root_.scala.tools.nsc.interpreter.`${INTERPRETER_IMPORT_LEVEL_UP}`\n")

    // add code for a new object to hold some imports
    def addWrapperCode(): Unit = {
      code append (request.wrapperDef(iw) + " {\n")
      trailingBraces append "}\n"+ request.postwrap +"\n"
      accessPath append s".$iw"
    }
    def addWrapper(): Unit =
      if (useMagicImport) {
        addLevelChangingImport()
      } else {
        addWrapperCode()
      }
      currentImps.clear()
    def maybeWrap(names: Name*) = if (names exists currentImps) addWrapper()

    // imports from Predef are relocated to the template header to allow hiding.
    def checkHeader(h: ImportHandler) = h.referencedNames contains PredefModule.name
    if (useMagicImport) code.append("\n") else addWrapperCode()

    // loop through previous requests, adding imports for each one
    // Reusing a single temporary value when import from a line with multiple definitions.
    val tempValLines = mutable.Set[Int]()
    for (ReqAndHandler(req, handler) <- reqsToUse) {
      val objName = req.lineRep.readPathInstance
//        if (isReplTrace) code.append(ss"// $objName definedNames ${handler.definedNames}, curImps $currentImps\n")
      handler match {
        case h: ImportHandler if checkHeader(h) =>
          header.clear()
          header append f"${h.member}%n"
        // If the user entered an import, then just use it; add an import wrapping
        // level if the import might conflict with some other import
        case x: ImportHandler if x.importsWildcard =>
          addWrapper()
          code append (x.member.toString + "\n")
          addWrapper()
        case x: ImportHandler =>
          maybeWrap(x.importedNames: _*)
          code append (x.member.toString + "\n")
          currentImps ++= x.importedNames

        case x if isClassBased =>
          for (sym <- x.definedSymbols) {
            maybeWrap(sym.name)
            x match {
              case _: ClassHandler =>
                code.append(s"import ${objName}${req.accessPath}.`${sym.name}`\n")
              case _ =>
                val valName = s"${req.lineRep.packageName}${req.lineRep.readName}"
                if (!tempValLines.contains(req.lineRep.lineId)) {
                  code.append(s"val $valName: ${objName}.type = $objName; ")
                  tempValLines += req.lineRep.lineId
                }
                code.append(s"import ${valName}${req.accessPath}.`${sym.name}`\n")
            }
            currentImps += sym.name
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

    addWrapperCode()

    val computedHeader = if (predefEscapes) header.toString else ""
    ComputedImports(computedHeader, code.toString, trailingBraces.toString, accessPath.toString)
  }

  private def allReqAndHandlers =
    prevRequestList flatMap (req => req.handlers map (req -> _))

  private def membersAtPickler(sym: Symbol): List[Symbol] =
    enteringPickler(sym.info.nonPrivateMembers.toList)
}
