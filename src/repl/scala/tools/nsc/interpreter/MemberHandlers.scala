/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import scala.language.implicitConversions

import scala.collection.mutable

trait MemberHandlers {
  val intp: IMain

  import intp.{ Request, global, naming }
  import global._
  import naming._

  private def codegenln(leadingPlus: Boolean, xs: String*): String = codegen(leadingPlus, (xs ++ Array("\n")): _*)
  private def codegenln(xs: String*): String = codegenln(true, xs: _*)
  private def codegen(leadingPlus: Boolean, xs: String*): String = {
    val front = if (leadingPlus) "+ " else ""
    front + (xs map string2codeQuoted mkString " + ")
  }
  private implicit def name2string(name: Name) = name.toString

  /** A traverser that finds all mentioned identifiers, i.e. things
   *  that need to be imported.  It might return extra names.
   */
  private class ImportVarsTraverser extends Traverser {
    val importVars = new mutable.HashSet[Name]()

    override def traverse(ast: Tree) = ast match {
      case Ident(name) =>
        // XXX this is obviously inadequate but it's going to require some effort
        // to get right.
        if (name.toString startsWith "x$") ()
        else {
          importVars += name
          // Needed to import `xxx` during line 2 of:
          //   scala> val xxx = ""
          //   scala> def foo: x<TAB>
          if (name.endsWith(IMain.DummyCursorFragment)) {
            val stripped = name.stripSuffix(IMain.DummyCursorFragment)
            importVars += stripped
          }
        }
      case _        => super.traverse(ast)
    }
  }
  private object ImportVarsTraverser {
    def apply(member: Tree) = {
      val ivt = new ImportVarsTraverser()
      ivt traverse member
      ivt.importVars.toList
    }
  }

  private def isTermMacro(ddef: DefDef): Boolean = ddef.mods.isMacro

  def chooseHandler(member: Tree): MemberHandler = member match {
    case member: DefDef if isTermMacro(member) => new TermMacroHandler(member)
    case member: DefDef                        => new DefHandler(member)
    case member: ValDef                        => new ValHandler(member)
    case member: ModuleDef                     => new ModuleHandler(member)
    case member: ClassDef                      => new ClassHandler(member)
    case member: TypeDef                       => new TypeAliasHandler(member)
    case member: Assign                        => new AssignHandler(member)
    case member: Import                        => new ImportHandler(member)
    case DocDef(_, documented)                 => chooseHandler(documented)
    case member                                => new GenericHandler(member)
  }

  sealed abstract class MemberDefHandler(override val member: MemberDef) extends MemberHandler(member) {
    override def name: Name = member.name
    def mods: Modifiers     = member.mods
    def keyword             = member.keyword
    def prettyName          = name.decode

    override def definesImplicit = member.mods.isImplicit
    override def definesTerm: Option[TermName] = Some(name.toTermName) filter (_ => name.isTermName)
    override def definesType: Option[TypeName] = Some(name.toTypeName) filter (_ => name.isTypeName)
    override def definedSymbols = if (symbol.exists) symbol :: Nil else Nil
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  sealed abstract class MemberHandler(val member: Tree) {
    def name: Name      = nme.NO_NAME
    def path            = intp.originalPath(symbol)
    def symbol          = if (member.symbol eq null) NoSymbol else member.symbol
    def definesImplicit = false
    def definesValue    = false

    def definesTerm     = Option.empty[TermName]
    def definesType     = Option.empty[TypeName]

    private lazy val _referencedNames = ImportVarsTraverser(member)
    def referencedNames = _referencedNames
    def importedNames   = List[Name]()
    def definedNames    = definesTerm.toList ++ definesType.toList
    def definedSymbols  = List[Symbol]()

    def extraCodeToEvaluate(req: Request): String = ""
    def resultExtractionCode(req: Request): String = ""

    private def shortName = this.getClass.toString split '.' last
    override def toString = shortName + referencedNames.mkString(" (refs: ", ", ", ")")
  }

  class GenericHandler(member: Tree) extends MemberHandler(member)

  import scala.io.AnsiColor.{ BOLD, BLUE, GREEN, RESET }

  def color(c: String, s: String) =
    if (replProps.colorOk) string2code(BOLD) + string2code(c) + s + string2code(RESET)
    else s

  def colorName(s: String) =
    color(BLUE, string2code(s))

  def colorType(s: String) =
    color(GREEN, string2code(s))

  class ValHandler(member: ValDef) extends MemberDefHandler(member) {
    val maxStringElements = 1000  // no need to mkString billions of elements
    override def definesValue = true

    override def resultExtractionCode(req: Request): String = {
      val isInternal = isUserVarName(name) && req.lookupTypeOf(name) == "Unit"
      if (!mods.isPublic || isInternal) ""
      else {
        // if this is a lazy val we avoid evaluating it here
        val resultString =
          if (mods.isLazy) codegenln(false, "<lazy>")
          else any2stringOf(path, maxStringElements)

        val vidString =
          if (replProps.vids) s"""" + f"@$${System.identityHashCode($path)}%8x" + """"
          else ""

        val nameString = colorName(prettyName) + vidString
        val typeString = colorType(req typeOf name)
        s""" + "$nameString: $typeString = " + $resultString"""
      }
    }
  }

  class DefHandler(member: DefDef) extends MemberDefHandler(member) {
    override def definesValue = flattensToEmpty(member.vparamss) // true if 0-arity
    override def resultExtractionCode(req: Request) = {
      val nameString = colorName(name)
      val typeString = colorType(req typeOf name)
      if (mods.isPublic) s""" + "$nameString: $typeString\\n"""" else ""
    }
  }

  abstract class MacroHandler(member: DefDef) extends MemberDefHandler(member) {
    override def referencedNames = super.referencedNames.flatMap(name => List(name.toTermName, name.toTypeName))
    override def definesValue = false
    override def definesTerm: Option[TermName] = Some(name.toTermName)
    override def definesType: Option[TypeName] = None
    override def resultExtractionCode(req: Request) = if (mods.isPublic) codegenln(notification(req)) else ""
    def notification(req: Request): String
  }

  class TermMacroHandler(member: DefDef) extends MacroHandler(member) {
    def notification(req: Request) = s"defined term macro $name: ${req.typeOf(name)}"
  }

  class AssignHandler(member: Assign) extends MemberHandler(member) {
    val Assign(lhs, rhs) = member
    override lazy val name = newTermName(freshInternalVarName())

    override def definesTerm = Some(name)
    override def definesValue = true
    override def extraCodeToEvaluate(req: Request) =
      """val %s = %s""".format(name, lhs)

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(req: Request) = {
      val lhsType = string2code(req lookupTypeOf name)
      val res     = string2code(req fullPath name)
      """ + "%s: %s = " + %s + "\n" """.format(string2code(lhs.toString), lhsType, res) + "\n"
    }
  }

  class ModuleHandler(module: ModuleDef) extends MemberDefHandler(module) {
    override def definesTerm = Some(name.toTermName)
    override def definesValue = true

    override def resultExtractionCode(req: Request) = codegenln("defined object ", name)
  }

  class ClassHandler(member: ClassDef) extends MemberDefHandler(member) {
    override def definedSymbols = List(symbol, symbol.companionSymbol) filterNot (_ == NoSymbol)
    override def definesType = Some(name.toTypeName)
    override def definesTerm = Some(name.toTermName) filter (_ => mods.isCase)

    override def resultExtractionCode(req: Request) =
      codegenln("defined %s %s".format(keyword, name))
  }

  class TypeAliasHandler(member: TypeDef) extends MemberDefHandler(member) {
    private def isAlias = mods.isPublic && treeInfo.isAliasTypeDef(member)
    override def definesType = Some(name.toTypeName) filter (_ => isAlias)

    override def resultExtractionCode(req: Request) =
      codegenln("defined type alias ", name) + "\n"
  }

  class ImportHandler(imp: Import) extends MemberHandler(imp) {
    val Import(expr, selectors) = imp
    def targetType = intp.global.rootMirror.getModuleIfDefined("" + expr) match {
      case NoSymbol => intp.typeOfExpression("" + expr)
      case sym      => sym.thisType
    }
    private def importableTargetMembers = importableMembers(targetType).toList
    // wildcard imports, e.g. import foo._
    private def selectorWild    = selectors filter (_.name == nme.USCOREkw)
    // renamed imports, e.g. import foo.{ bar => baz }
    private def selectorRenames = selectors map (_.rename) filterNot (_ == null)

    /** Whether this import includes a wildcard import */
    val importsWildcard = selectorWild.nonEmpty

    def implicitSymbols = importedSymbols filter (_.isImplicit)
    def importedSymbols = individualSymbols ++ wildcardSymbols

    private val selectorNames = selectorRenames filterNot (_ == nme.USCOREkw) flatMap (_.bothNames) toSet
    lazy val individualSymbols: List[Symbol] = exitingTyper(importableTargetMembers filter (m => selectorNames(m.name)))
    lazy val wildcardSymbols: List[Symbol]   = exitingTyper(if (importsWildcard) importableTargetMembers else Nil)

    /** Complete list of names imported by a wildcard */
    lazy val wildcardNames: List[Name]   = wildcardSymbols map (_.name)
    lazy val individualNames: List[Name] = individualSymbols map (_.name)

    /** The names imported by this statement */
    override lazy val importedNames: List[Name] = wildcardNames ++ individualNames
    lazy val importsSymbolNamed: Set[String] = importedNames map (_.toString) toSet

    def importString = imp.toString
    override def resultExtractionCode(req: Request) = codegenln(importString) + "\n"
  }
}
