/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package interpreter

import scala.collection.{ mutable, immutable }
import scala.PartialFunction.cond
import scala.reflect.NameTransformer
import util.Chars

trait MemberHandlers {
  val intp: IMain

  import intp.{ Request, global, naming }
  import global._
  import naming._

  def string2codeQuoted(str: String) = "\"" + string2code(str) + "\""

  /** Convert a string into code that can recreate the string.
   *  This requires replacing all special characters by escape
   *  codes. It does not add the surrounding " marks.  */
  def string2code(str: String): String = {
    val res = new StringBuilder
    for (c <- str) c match {
      case '"' | '\'' | '\\'  => res += '\\' ; res += c
      case _ if c.isControl   => res ++= Chars.char2uescape(c)
      case _                  => res += c
    }
    res.toString
  }
  def any2stringOf(x: Any, maxlen: Int) =
    "scala.runtime.ScalaRunTime.stringOf(%s, %s)".format(x, maxlen)

  private def codegenln(leadingPlus: Boolean, xs: String*): String = codegen(leadingPlus, (xs ++ Array("\n")): _*)
  private def codegenln(xs: String*): String = codegenln(true, xs: _*)

  private def codegen(xs: String*): String = codegen(true, xs: _*)
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
        else importVars += name
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

  def chooseHandler(member: Tree): MemberHandler = member match {
    case member: DefDef               => new DefHandler(member)
    case member: ValDef               => new ValHandler(member)
    case member@Assign(Ident(_), _)   => new AssignHandler(member)
    case member: ModuleDef            => new ModuleHandler(member)
    case member: ClassDef             => new ClassHandler(member)
    case member: TypeDef              => new TypeAliasHandler(member)
    case member: Import               => new ImportHandler(member)
    case DocDef(_, documented)        => chooseHandler(documented)
    case member                       => new GenericHandler(member)
  }

  sealed abstract class MemberDefHandler(override val member: MemberDef) extends MemberHandler(member) {
    def name: Name = member.name
    def mods: Modifiers = member.mods
    def keyword = member.keyword
    def prettyName = NameTransformer.decode(name)

    override def definesImplicit = member.mods.isImplicit
    override def definesTerm: Option[TermName] = Some(name.toTermName) filter (_ => name.isTermName)
    override def definesType: Option[TypeName] = Some(name.toTypeName) filter (_ => name.isTypeName)
  }

  /** Class to handle one member among all the members included
   *  in a single interpreter request.
   */
  sealed abstract class MemberHandler(val member: Tree) {
    def tpe = member.tpe
    def definesImplicit = false
    def definesValue    = false

    def definesTerm     = Option.empty[TermName]
    def definesType     = Option.empty[TypeName]

    lazy val referencedNames = ImportVarsTraverser(member)
    def importedNames        = List[Name]()
    def definedNames         = definesTerm.toList ++ definesType.toList
    def definedOrImported    = definedNames ++ importedNames

    def extraCodeToEvaluate(req: Request): String = ""
    def resultExtractionCode(req: Request): String = ""

    private def shortName = this.getClass.toString split '.' last
    override def toString = shortName + referencedNames.mkString(" (refs: ", ", ", ")")
  }

  class GenericHandler(member: Tree) extends MemberHandler(member)

  class ValHandler(member: ValDef) extends MemberDefHandler(member) {
    val maxStringElements = 1000  // no need to mkString billions of elements
    def stringOf(x: Any) = any2stringOf(x, maxStringElements)
    override def definesValue = true

    override def resultExtractionCode(req: Request): String = {
      val isInternal = isUserVarName(name) && req.lookupTypeOf(name) == "Unit"
      if (!mods.isPublic || isInternal) ""
      else {
        // if this is a lazy val we avoid evaluating it here
        val resultString =
          if (mods.isLazy) codegenln(false, "<lazy>")
          else stringOf(req fullPath name)

        """ + "%s: %s = " + %s""".format(prettyName, string2code(req typeOf name), resultString)
      }
    }
  }

  class DefHandler(member: DefDef) extends MemberDefHandler(member) {
    private def vparamss = member.vparamss
    // true if 0-arity
    override def definesValue = vparamss.isEmpty || vparamss.head.isEmpty
    override def resultExtractionCode(req: Request) =
      if (mods.isPublic) codegenln(name, ": ", req.typeOf(name)) else ""
  }

  class AssignHandler(member: Assign) extends MemberHandler(member) {
    val lhs = member.lhs.asInstanceOf[Ident] // an unfortunate limitation
    val name = newTermName(freshInternalVarName())

    override def definesTerm = Some(name)
    override def definesValue = true
    override def extraCodeToEvaluate(req: Request) =
      """val %s = %s""".format(name, lhs)

    /** Print out lhs instead of the generated varName */
    override def resultExtractionCode(req: Request) = {
      val lhsType = string2code(req lookupTypeOf name)
      val res = string2code(req fullPath name)

      """ + "%s: %s = " + %s + "\n" """.format(lhs, lhsType, res) + "\n"
    }
  }

  class ModuleHandler(module: ModuleDef) extends MemberDefHandler(module) {
    override def definesTerm = Some(name)
    override def definesValue = true

    override def resultExtractionCode(req: Request) = codegenln("defined module ", name)
  }

  class ClassHandler(member: ClassDef) extends MemberDefHandler(member) {
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
    def targetType = intp.typeOfExpression("" + expr)

    private def selectorWild    = selectors filter (_.name == nme.USCOREkw)   // wildcard imports, e.g. import foo._
    private def selectorRenames = selectors map (_.rename) filterNot (_ == null)

    /** Whether this import includes a wildcard import */
    val importsWildcard = selectorWild.nonEmpty

    /** Complete list of names imported by a wildcard */
    def wildcardImportedNames: List[Name] = (
      for (tpe <- targetType ; if importsWildcard) yield
        tpe.nonPrivateMembers filter (x => x.isMethod && x.isPublic) map (_.name) distinct
    ).toList.flatten

    /** The individual names imported by this statement */
    /** XXX come back to this and see what can be done with wildcards now that
     *  we know how to enumerate the identifiers.
     */
    override lazy val importedNames: List[Name] =
      selectorRenames filterNot (_ == nme.USCOREkw) flatMap (_.bothNames)

    override def resultExtractionCode(req: Request) = codegenln(imp.toString) + "\n"
  }
}
