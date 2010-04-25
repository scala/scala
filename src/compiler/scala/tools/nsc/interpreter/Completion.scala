/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author Paul Phillips
 */


package scala.tools.nsc
package interpreter

import jline._
import java.net.URL
import java.util.{ List => JList }
import java.lang.reflect
import scala.tools.util.PathResolver
import io.{ Path, Directory }

object Completion {
  def looksLikeInvocation(code: String) = (
        (code != null)
    &&  (code startsWith ".")
    && !(code == ".")
    && !(code startsWith "./")
    && !(code startsWith "..")
  )

  object Forwarder {
    def apply(forwardTo: () => Option[CompletionAware]): CompletionAware = new CompletionAware {
      def completions() = forwardTo() map (_.completions()) getOrElse Nil
      override def follow(s: String) = forwardTo() flatMap (_ follow s)
    }
  }
}
import Completion._

// REPL completor - queries supplied interpreter for valid
// completions based on current contents of buffer.
class Completion(val repl: Interpreter) {
  // verbosity goes up with consecutive tabs
  private var verbosity: Int = 0
  def resetVerbosity() = verbosity = 0

  def isCompletionDebug = repl.isCompletionDebug
  def DBG(msg: => Any) = if (isCompletionDebug) println(msg.toString)

  lazy val global: repl.compiler.type = repl.compiler
  import global._
  import definitions.{ PredefModule, RootClass, AnyClass, AnyRefClass, ScalaPackage, JavaLangPackage }

  // XXX not yet used.
  lazy val dottedPaths = {
    def walk(tp: Type): scala.List[Symbol] = {
      val pkgs = tp.nonPrivateMembers filter (_.isPackage)
      pkgs ++ (pkgs map (_.tpe) flatMap walk)
    }
    walk(RootClass.tpe)
  }

  def getType(name: String, isModule: Boolean) = {
    val f = if (isModule) definitions.getModule(_: Name) else definitions.getClass(_: Name)
    try Some(f(name).tpe)
    catch { case _: MissingRequirementError => None }
  }

  def typeOf(name: String) = getType(name, false)
  def moduleOf(name: String) = getType(name, true)

  trait CompilerCompletion {
    def tp: Type
    def effectiveTp = tp match {
      case MethodType(Nil, resType) => resType
      case _                        => tp
    }

    // for some reason any's members don't show up in subclasses, which
    // we need so 5.<tab> offers asInstanceOf etc.
    private def anyMembers = AnyClass.tpe.nonPrivateMembers
    def anyRefMethodsToShow = List("isInstanceOf", "asInstanceOf", "toString")

    def tos(sym: Symbol) = sym.name.decode.toString
    def memberNamed(s: String) = members find (x => tos(x) == s)
    def hasMethod(s: String) = methods exists (x => tos(x) == s)

    def members     = (effectiveTp.nonPrivateMembers ++ anyMembers) filter (_.isPublic)
    def methods     = members filter (_.isMethod)
    def packages    = members filter (_.isPackage)
    def aliases     = members filter (_.isAliasType)

    def memberNames   = members map tos
    def methodNames   = methods map tos
    def packageNames  = packages map tos
    def aliasNames    = aliases map tos
  }

  object TypeMemberCompletion {
    def apply(tp: Type): TypeMemberCompletion = {
      if (tp.typeSymbol.isPackageClass) new PackageCompletion(tp)
      else new TypeMemberCompletion(tp)
    }
    def imported(tp: Type) = new ImportCompletion(tp)
  }

  class TypeMemberCompletion (val tp: Type) extends CompletionAware with CompilerCompletion {
    def excludeEndsWith: List[String] = Nil
    def excludeStartsWith: List[String] = List("<") // <byname>, <repeated>, etc.
    def excludeNames: List[String] = anyref.methodNames -- anyRefMethodsToShow ++ List("_root_")

    def exclude(name: String): Boolean = (
      (name contains "$") ||
      (excludeNames contains name) ||
      (excludeEndsWith exists (name endsWith _)) ||
      (excludeStartsWith exists (name startsWith _))
    )
    def filtered(xs: List[String]) = xs filterNot exclude distinct
    def completions = {
      returning(filtered(memberNames))(xs => DBG("%s completions: %s".format(tp, xs)))
    }

    override def follow(s: String): Option[CompletionAware] =
      memberNamed(s) map (x => TypeMemberCompletion(x.tpe))

    override def toString = "TypeMemberCompletion(%s)".format(tp)
  }

  class PackageCompletion(tp: Type) extends TypeMemberCompletion(tp) {
    override def excludeNames = anyref.methodNames
  }

  class LiteralCompletion(lit: Literal) extends TypeMemberCompletion(lit.value.tpe) {
    private lazy val completions0 = filtered(memberNames)
    private lazy val completions1 = memberNames
    override def completions      = if (verbosity == 0) completions0 else completions1
  }

  class ImportCompletion(tp: Type) extends TypeMemberCompletion(tp) {
    private lazy val completions0 = filtered(methods filterNot (_.isSetter) map tos)
    private lazy val completions1 = super.completions
    override def completions      = if (verbosity == 0) completions0 else completions1
  }

  // not for completion but for excluding
  object anyref extends TypeMemberCompletion(AnyRefClass.tpe) { }

  // the unqualified vals/defs/etc visible in the repl
  object ids extends CompletionAware {
    def completions() = repl.unqualifiedIds ::: List("classOf")
    // we try to use the compiler and fall back on reflection if necessary
    // (which at present is for anything defined in the repl session.)
    override def follow(id: String) =
      if (completions contains id) {
        for (clazz <- repl clazzForIdent id) yield {
          (typeOf(clazz.getName) map TypeMemberCompletion.apply) getOrElse new InstanceCompletion(clazz)
        }
      }
      else None
  }

  // wildcard imports in the repl like "import global._" or "import String._"
  private def imported = repl.wildcardImportedTypes map TypeMemberCompletion.imported

  // literal Ints, Strings, etc.
  object literals extends CompletionAware {
    def simpleParse(code: String): Tree = {
      val unit    = new CompilationUnit(new util.BatchSourceFile("<console>", code))
      val scanner = new syntaxAnalyzer.UnitParser(unit)
      val tss     = scanner.templateStatSeq(false)._2

      if (tss.size == 1) tss.head else EmptyTree
    }

    val completions = Nil

    override def follow(id: String) = simpleParse(id) match {
      case x: Literal   => Some(new LiteralCompletion(x))
      case _            => None
    }
  }

  // top level packages
  object rootClass extends TypeMemberCompletion(RootClass.tpe) { }
  // members of Predef
  object predef extends TypeMemberCompletion(PredefModule.tpe) {
    override def excludeEndsWith    = super.excludeEndsWith ++ List("Wrapper", "ArrayOps")
    override def excludeStartsWith  = super.excludeStartsWith ++ List("wrap")
    override def excludeNames       = anyref.methodNames

    override def exclude(name: String) = super.exclude(name) || (
      (name contains "2")
    )

    private lazy val completions0 = Nil
    private lazy val completions1 = super.completions
    override def completions = if (verbosity == 0) completions0 else completions1
  }
  // members of scala.*
  object scalalang extends PackageCompletion(ScalaPackage.tpe) {
    def arityClasses = List("Product", "Tuple", "Function")
    def skipArity(name: String) = arityClasses exists (x => name != x && (name startsWith x))
    override def exclude(name: String) = super.exclude(name) || (
      skipArity(name)
    )

    private lazy val completions0 = filtered(packageNames ++ aliasNames)
    private lazy val completions1 = super.completions
    override def completions = if (verbosity == 0) completions0 else completions1
  }
  // members of java.lang.*
  object javalang extends PackageCompletion(JavaLangPackage.tpe) {
    override lazy val excludeEndsWith   = super.excludeEndsWith ++ List("Exception", "Error")
    override lazy val excludeStartsWith = super.excludeStartsWith ++ List("CharacterData")

    private lazy val completions0 = filtered(packageNames)
    private lazy val completions1 = super.completions
    override def completions = if (verbosity == 0) completions0 else completions1
  }

  // the list of completion aware objects which should be consulted
  lazy val topLevelBase: List[CompletionAware] = List(ids, rootClass, predef, scalalang, javalang, literals)
  def topLevel = topLevelBase ++ imported

  // the first tier of top level objects (doesn't include file completion)
  def topLevelFor(parsed: Parsed) = topLevel flatMap (_ completionsFor parsed)

  // the most recent result
  def lastResult = Forwarder(() => ids follow repl.mostRecentVar)

  def lastResultFor(parsed: Parsed) = {
    /** The logic is a little tortured right now because normally '.' is
     *  ignored as a delimiter, but on .<tab> it needs to be propagated.
     */
    val xs = lastResult completionsFor parsed
    if (parsed.isEmpty) xs map ("." + _) else xs
  }

  // chasing down results which won't parse
  def execute(line: String): Option[Any] = {
    val parsed = Parsed(line)
    def noDotOrSlash = line forall (ch => ch != '.' && ch != '/')

    if (noDotOrSlash) None  // we defer all unqualified ids to the repl.
    else {
      (ids executionFor parsed) orElse
      (rootClass executionFor parsed) orElse
      (FileCompletion executionFor line)
    }
  }

  // generic interface for querying (e.g. interpreter loop, testing)
  def completions(buf: String): List[String] =
    topLevelFor(Parsed.dotted(buf + ".", buf.length + 1))

  // jline's entry point
  lazy val jline: ArgumentCompletor =
    returning(new ArgumentCompletor(new JLineCompletion, new JLineDelimiter))(_ setStrict false)

  class JLineCompletion extends Completor {
    // For recording the buffer on the last tab hit
    private var lastTab: (String, Int) = ("", -1)

    // Does this represent two consecutive tabs?
    def isConsecutiveTabs(current: (String, Int)) = current == lastTab

    // This is jline's entry point for completion.
    override def complete(buf: String, cursor: Int, candidates: JList[String]): Int = {
      verbosity = if (isConsecutiveTabs((buf, cursor))) verbosity + 1 else 0
      DBG("complete(%s, %d), verbosity: %s".format(buf, cursor, verbosity))

      lastTab = (buf, cursor)

      // we don't try lower priority completions unless higher ones return no results.
      def tryCompletion(p: Parsed, completionFunction: Parsed => List[String]): Option[Int] = {
        completionFunction(p) match {
          case Nil  => None
          case xs   =>
            // modify in place and return the position
            xs foreach (candidates add _)
            // DBG("Completion candidates: " + (xs mkString " "))

            Some(p.position)
        }
      }

      // a single dot is special cased to completion on the previous result
      def lastResultCompletion =
        if (!looksLikeInvocation(buf)) None
        else tryCompletion(Parsed.dotted(buf drop 1, cursor), lastResultFor)

      def regularCompletion = tryCompletion(Parsed.dotted(buf, cursor), topLevelFor)
      def fileCompletion    = tryCompletion(Parsed.undelimited(buf, cursor), FileCompletion completionsFor _.buffer)

      (lastResultCompletion orElse regularCompletion orElse fileCompletion) getOrElse cursor
    }
  }
}
