/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Paul Phillips
 */

package scala.tools.nsc
package typechecker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Exception.ultimately
import symtab.Flags._
import PartialFunction._
import scala.annotation.tailrec

/** An interface to enable higher configurability of diagnostic messages
 *  regarding type errors.  This is barely a beginning as error messages are
 *  distributed far and wide across the codebase.  The plan is to partition
 *  error messages into some broad groups and provide some mechanism for
 *  being more or less verbose on a selective basis.  Possible groups include
 *  such examples as
 *
 *    arity errors
 *    kind errors
 *    variance errors
 *    ambiguity errors
 *    volatility/stability errors
 *    implementation restrictions
 *
 *  And more, and there is plenty of overlap, so it'll be a process.
 *
 *  @author Paul Phillips
 *  @version 1.0
 */
trait TypeDiagnostics {
  self: Analyzer with StdAttachments =>

  import global._
  import definitions._

  /** For errors which are artifacts of the implementation: such messages
   *  indicate that the restriction may be lifted in the future.
   */
  def restrictionWarning(pos: Position, unit: CompilationUnit, msg: String): Unit =
    reporter.warning(pos, "Implementation restriction: " + msg)
  def restrictionError(pos: Position, unit: CompilationUnit, msg: String): Unit =
    reporter.error(pos, "Implementation restriction: " + msg)

  /** A map of Positions to addendums - if an error involves a position in
   *  the map, the addendum should also be printed.
   */
  private val addendums = perRunCaches.newMap[Position, () => String]()
  private var isTyperInPattern = false

  /** Devising new ways of communicating error info out of
   *  desperation to work on error messages.  This is used
   *  by typedPattern to wrap its business so we can generate
   *  a sensible error message when things go south.
   */
  def typingInPattern[T](body: => T): T = {
    val saved = isTyperInPattern
    isTyperInPattern = true
    try body
    finally isTyperInPattern = saved
  }

  def setAddendum(pos: Position, msg: () => String) =
    if (pos != NoPosition)
      addendums(pos) = msg

  def withAddendum(pos: Position) = (_: String) + addendums.getOrElse(pos, () => "")()

  def decodeWithKind(name: Name, owner: Symbol): String = {
    val prefix = (
      if (name.isTypeName) "type "
      else if (owner.isPackageClass) "object "
      else "value "
    )
    prefix + name.decode
  }

  private def atBounded(t: Tree) = t.hasAttachment[AtBoundIdentifierAttachment.type]

  /** Does the positioned line assigned to t1 precede that of t2?
   */
  def posPrecedes(p1: Position, p2: Position) = p1.isDefined && p2.isDefined && p1.line < p2.line
  def linePrecedes(t1: Tree, t2: Tree) = posPrecedes(t1.pos, t2.pos)

  private object DealiasedType extends TypeMap {
    def apply(tp: Type): Type = tp match {
      // Avoid "explaining" that String is really java.lang.String,
      // while still dealiasing types from non-default namespaces.
      case TypeRef(pre, sym, args) if sym.isAliasType && !sym.isInDefaultNamespace =>
        mapOver(tp.dealias)
      case _ =>
        mapOver(tp)
    }
  }

  /** An explanatory note to be added to error messages
   *  when there's a problem with abstract var defs */
  def abstractVarMessage(sym: Symbol): String =
    if (sym.isSetter || sym.isGetter && sym.setterIn(sym.owner).exists)
      "\n(Note that variables need to be initialized to be defined)"
    else ""

  private def methodTypeErrorString(tp: Type) = tp match {
    case mt @ MethodType(params, resultType)  =>
      def forString = params map (_.defString)

       forString.mkString("(", ",", ")") + resultType
    case x                                    => x.toString
  }

  /**
   * [a, b, c] => "(a, b, c)"
   * [a, B]    => "(param1, param2)"
   * [a, B, c] => "(param1, ..., param2)"
   */
  final def exampleTuplePattern(names: List[Name]): String = {
    val arity = names.length
    val varPatternNames: Option[List[String]] = sequence(names map {
      case name if nme.isVariableName(name) => Some(name.decode)
      case _                                => None
    })
    def parenthesize(a: String) = s"($a)"
    def genericParams = (Seq("param1") ++ (if (arity > 2) Seq("...") else Nil) ++ Seq(s"param$arity"))
    parenthesize(varPatternNames.getOrElse(genericParams).mkString(", "))
  }

  def alternatives(tree: Tree): List[Type] = tree.tpe match {
    case OverloadedType(pre, alternatives)  => alternatives map pre.memberType
    case _                                  => Nil
  }
  def alternativesString(tree: Tree) =
    alternatives(tree) map (x => "  " + methodTypeErrorString(x)) mkString ("", " <and>\n", "\n")

  /** The symbol which the given accessor represents (possibly in part).
    * This is used for error messages, where we want to speak in terms
    * of the actual declaration or definition, not in terms of the generated setters
    * and getters.
    *
    * TODO: is it wise to create new symbols simply to generate error message? is this safe in interactive/resident mode?
    */
  def underlyingSymbol(member: Symbol): Symbol =
    if (!member.hasAccessorFlag || member.accessed == NoSymbol) member
    else if (!member.isDeferred) member.accessed
    else {
      val getter = if (member.isSetter) member.getterIn(member.owner) else member
      val flags  = if (getter.setterIn(member.owner) != NoSymbol) DEFERRED.toLong | MUTABLE else DEFERRED

      getter.owner.newValue(getter.name.toTermName, getter.pos, flags) setInfo getter.tpe.resultType
    }

  def treeSymTypeMsg(tree: Tree): String = {
    val sym               = tree.symbol
    def hasParams         = tree.tpe.paramSectionCount > 0
    def preResultString   = if (hasParams) ": " else " of type "

    def patternMessage    = "pattern " + tree.tpe.finalResultType + valueParamsString(tree.tpe)
    def exprMessage       = "expression of type " + tree.tpe
    def overloadedMessage = s"overloaded method $sym with alternatives:\n" + alternativesString(tree)
    def moduleMessage     = "" + sym
    def defaultMessage    = moduleMessage + preResultString + tree.tpe
    def applyMessage      = defaultMessage + tree.symbol.locationString

    if (!tree.hasExistingSymbol) {
      if (isTyperInPattern) patternMessage
      else exprMessage
    }
    else if (sym.isOverloaded) overloadedMessage
    else if (sym.isModule) moduleMessage
    else if (sym.name == nme.apply) applyMessage
    else defaultMessage
  }

  def disambiguate(ss: List[String]) = ss match {
    case Nil      => Nil
    case s :: ss  => s :: (ss map { case `s` => "(some other)"+s ; case x => x })
  }

  // todo: use also for other error messages
  def existentialContext(tp: Type) = tp.skolemsExceptMethodTypeParams match {
    case Nil  => ""
    case xs   => " where " + (disambiguate(xs map (_.existentialToString)) mkString ", ")
  }

  def explainAlias(tp: Type) = {
    // Don't automatically normalize standard aliases; they still will be
    // expanded if necessary to disambiguate simple identifiers.
    val deepDealias = DealiasedType(tp)
    if (tp eq deepDealias) "" else {
      // A sanity check against expansion being identical to original.
      val s = "" + deepDealias
      if (s == "" + tp) ""
      else "\n    (which expands to)  " + s
    }
  }

  /** Look through the base types of the found type for any which
   *  might have been valid subtypes if given conformant type arguments.
   *  Examine those for situations where the type error would have been
   *  eliminated if the variance were different.  In such cases, append
   *  an additional explanatory message.
   *
   *  TODO: handle type aliases better.
   */
  def explainVariance(found: Type, req: Type): String = {
    found.baseTypeSeq.toList foreach { tp =>
      if (tp.typeSymbol isSubClass req.typeSymbol) {
        val foundArgs = tp.typeArgs
        val reqArgs   = req.typeArgs
        val params    = req.typeConstructor.typeParams

        if (foundArgs.nonEmpty && foundArgs.length == reqArgs.length) {
          val relationships = (foundArgs, reqArgs, params).zipped map {
            case (arg, reqArg, param) =>
              def mkMsg(isSubtype: Boolean) = {
                val op      = if (isSubtype) "<:" else ">:"
                val suggest = if (isSubtype) "+" else "-"
                val reqsym  = req.typeSymbol
                def isJava  = reqsym.isJavaDefined
                def isScala = reqsym hasTransOwner ScalaPackageClass

                val explainFound = "%s %s %s%s, but ".format(
                  arg, op, reqArg,
                  // If the message involves a type from the base type sequence rather than the
                  // actual found type, we need to explain why we're talking about it.  Less brute
                  // force measures than comparing normalized Strings were producing error messages
                  // like "and java.util.ArrayList[String] <: java.util.ArrayList[String]" but there
                  // should be a cleaner way to do this.
                  if (found.dealiasWiden.toString == tp.dealiasWiden.toString) ""
                  else " (and %s <: %s)".format(found, tp)
                )
                val explainDef = {
                  val prepend = if (isJava) "Java-defined " else ""
                  "%s%s is %s in %s.".format(prepend, reqsym, param.variance, param)
                }
                // Don't suggest they change the class declaration if it's somewhere
                // under scala.* or defined in a java class, because attempting either
                // would be fruitless.
                val suggestChange = "\nYou may wish to " + (
                  if (isScala || isJava)
                    "investigate a wildcard type such as `_ %s %s`. (SLS 3.2.10)".format(op, reqArg)
                  else
                    "define %s as %s%s instead. (SLS 4.5)".format(param.name, suggest, param.name)
                )

                Some("Note: " + explainFound + explainDef + suggestChange)
              }
              // In these cases the arg is OK and needs no explanation.
              val conforms = (
                   (arg =:= reqArg)
                || ((arg <:< reqArg) && param.isCovariant)
                || ((reqArg <:< arg) && param.isContravariant)
              )
              val invariant = param.variance.isInvariant

              if (conforms)                             Some("")
              else if ((arg <:< reqArg) && invariant)   mkMsg(isSubtype = true)   // covariant relationship
              else if ((reqArg <:< arg) && invariant)   mkMsg(isSubtype = false)  // contravariant relationship
              else None // we assume in other cases our ham-fisted advice will merely serve to confuse
          }
          val messages = relationships.flatten
          // the condition verifies no type argument came back None
          if (messages.size == foundArgs.size)
            return messages filterNot (_ == "") mkString ("\n", "\n", "")
        }
      }
    }
    ""    // no elaborable variance situation found
  }

  // For found/required errors where AnyRef would have sufficed:
  // explain in greater detail.
  def explainAnyVsAnyRef(found: Type, req: Type): String = {
    if (AnyRefTpe <:< req) notAnyRefMessage(found) else ""
  }

  def finalOwners(tpe: Type): Boolean = (tpe.prefix == NoPrefix) || recursivelyFinal(tpe)

  @tailrec
  final def recursivelyFinal(tpe: Type): Boolean = {
    val prefix = tpe.prefix
    if (prefix != NoPrefix) {
      if (prefix.typeSymbol.isFinal) {
        recursivelyFinal(prefix)
      } else {
        false
      }
    } else {
      true
    }
  }

  // TODO - figure out how to avoid doing any work at all
  // when the message will never be seen.  I though context.reportErrors
  // being false would do that, but if I return "<suppressed>" under
  // that condition, I see it.
  def foundReqMsg(found: Type, req: Type): String = {
    val foundWiden = found.widen
    val reqWiden = req.widen
    val sameNamesDifferentPrefixes =
      foundWiden.typeSymbol.name == reqWiden.typeSymbol.name &&
        foundWiden.prefix.typeSymbol != reqWiden.prefix.typeSymbol
    val easilyMistakable =
      sameNamesDifferentPrefixes &&
      !req.typeSymbol.isConstant &&
      finalOwners(foundWiden) && finalOwners(reqWiden) &&
      !found.typeSymbol.isTypeParameterOrSkolem && !req.typeSymbol.isTypeParameterOrSkolem

    if (easilyMistakable) {
      val longestNameLength = foundWiden.nameAndArgsString.length max reqWiden.nameAndArgsString.length
      val paddedFoundName = foundWiden.nameAndArgsString.padTo(longestNameLength, ' ')
      val paddedReqName = reqWiden.nameAndArgsString.padTo(longestNameLength, ' ')
      ";\n found   : " + (paddedFoundName + s" (in ${found.prefix.typeSymbol.fullNameString}) ") + explainAlias(found) +
       "\n required: " + (paddedReqName + s" (in ${req.prefix.typeSymbol.fullNameString}) ") + explainAlias(req)
    } else {
      def baseMessage = {
        ";\n found   : " + found.toLongString + existentialContext(found) + explainAlias(found) +
         "\n required: " + req + existentialContext(req) + explainAlias(req)
      }
      (withDisambiguation(Nil, found, req)(baseMessage)
        + explainVariance(found, req)
        + explainAnyVsAnyRef(found, req)
        )
    }
  }

  def typePatternAdvice(sym: Symbol, ptSym: Symbol) = {
    val clazz = if (sym.isModuleClass) sym.companionClass else sym
    val caseString =
      if (clazz.isCaseClass && (clazz isSubClass ptSym))
        ( clazz.caseFieldAccessors
          map (_ => "_")    // could use the actual param names here
          mkString (s"`case ${clazz.name}(", ",", ")`")
        )
      else
        "`case _: " + (clazz.typeParams match {
          case Nil  => "" + clazz.name
          case xs   => xs map (_ => "_") mkString (clazz.name + "[", ",", "]")
        })+ "`"

    if (!clazz.exists) ""
    else "\nNote: if you intended to match against the class, try "+ caseString
  }

  case class TypeDiag(tp: Type, sym: Symbol) extends Ordered[TypeDiag] {
    // save the name because it will be mutated until it has been
    // distinguished from the other types in the same error message
    private val savedName = sym.name
    private var postQualifiedWith: List[Symbol] = Nil
    def restoreName()     = sym.name = savedName
    def modifyName(f: String => String) = sym setName newTypeName(f(sym.name.toString))

    // functions to manipulate the name
    def preQualify()   = modifyName(trueOwner.fullName + "." + _)
    def postQualify()  = if (!(postQualifiedWith contains trueOwner)) {
      postQualifiedWith ::= trueOwner
      modifyName(s => s"$s(in $trueOwner)")
    }
    def typeQualify()  = if (sym.isTypeParameterOrSkolem) postQualify()
    def nameQualify()  = if (trueOwner.isPackageClass) preQualify() else postQualify()

    def trueOwner  = tp.typeSymbol.effectiveOwner
    def aliasOwner = tp.typeSymbolDirect.effectiveOwner

    def sym_==(other: TypeDiag)     = tp.typeSymbol == other.tp.typeSymbol
    def owner_==(other: TypeDiag)   = trueOwner == other.trueOwner
    def string_==(other: TypeDiag)  = tp.toString == other.tp.toString
    def name_==(other: TypeDiag)    = sym.name == other.sym.name

    def compare(other: TypeDiag) =
      if (this == other) 0
      else if (sym isLess other.sym) -1
      else 1

    override def toString = {
      """
      |tp = %s
      |tp.typeSymbol = %s
      |tp.typeSymbol.owner = %s
      |tp.typeSymbolDirect = %s
      |tp.typeSymbolDirect.owner = %s
      """.stripMargin.format(
        tp, tp.typeSymbol, tp.typeSymbol.owner, tp.typeSymbolDirect, tp.typeSymbolDirect.owner
      )
    }
  }
  /** This is tricky stuff - we need to traverse types deeply to
   *  explain name ambiguities, which may occur anywhere.  However
   *  when lub explosions come through it knocks us into an n^2
   *  disaster, see scala/bug#5580.  This is trying to perform the initial
   *  filtering of possibly ambiguous types in a sufficiently
   *  aggressive way that the state space won't explode.
   */
  private def typeDiags(locals: List[Symbol], types0: Type*): List[TypeDiag] = {
    val types   = types0.toList
    // If two different type diag instances are seen for a given
    // key (either the string representation of a type, or the simple
    // name of a symbol) then keep them for disambiguation.
    val strings = mutable.Map[String, Set[TypeDiag]]() withDefaultValue Set()
    val names   = mutable.Map[Name, Set[TypeDiag]]() withDefaultValue Set()

    val localsSet = locals.toSet

    def record(t: Type, sym: Symbol) = {
      if (!localsSet(sym)) {
        val diag = TypeDiag(t, sym)
        strings("" + t) += diag
        names(sym.name) += diag
      }
    }
    for (tpe <- types ; t <- tpe) {
      t match {
        case ConstantType(_)    => record(t, t.underlying.typeSymbol)
        case TypeRef(_, sym, _) => record(t, sym)
        case _                  => ()
      }
    }

    val collisions = strings.values ++ names.values filter (_.size > 1)
    collisions.flatten.toList
  }

  /** The distinct pairs from an ordered list. */
  private def pairs[T <: Ordered[T]](xs: Seq[T]): Seq[(T, T)] = {
    for (el1 <- xs ; el2 <- xs ; if el1 < el2) yield
      ((el1, el2))
  }

  /** Given any number of types, alters the name information in the symbols
   *  until they can be distinguished from one another: then executes the given
   *  code.  The names are restored and the result is returned.
   */
  def withDisambiguation[T](locals: List[Symbol], types: Type*)(op: => T): T = {
    val typeRefs = typeDiags(locals, types: _*)
    val toCheck  = pairs(typeRefs) filterNot { case (td1, td2) => td1 sym_== td2 }

    ultimately(typeRefs foreach (_.restoreName())) {
      for ((td1, td2) <- toCheck) {
        val tds = List(td1, td2)

        // If the types print identically, qualify them:
        //   a) If the dealiased owner is a package, the full path
        //   b) Otherwise, append (in <owner>)
        if (td1 string_== td2)
          tds foreach (_.nameQualify())

        // If they still print identically:
        //   a) If they are type parameters with different owners, append (in <owner>)
        //   b) Failing that, the best we can do is append "(some other)" to the latter.
        if (td1 string_== td2) {
          if (td1 owner_== td2)
            td2.modifyName("(some other)" + _)
          else
            tds foreach (_.typeQualify())
        }
      }
      // performing the actual operation
      op
    }
  }

  trait TyperDiagnostics {
    self: Typer =>

    def permanentlyHiddenWarning(pos: Position, hidden: Name, defn: Symbol) =
      context.warning(pos, "imported `%s' is permanently hidden by definition of %s".format(hidden, defn.fullLocationString))

    object checkUnused {
      val ignoreNames: Set[TermName] = Set(
        "readResolve", "readObject", "writeObject", "writeReplace"
      ).map(TermName(_))

      class UnusedPrivates extends Traverser {
        val defnTrees = ListBuffer[MemberDef]()
        val targets   = mutable.Set[Symbol]()
        val setVars   = mutable.Set[Symbol]()
        val treeTypes = mutable.Set[Type]()
        val atBounds  = mutable.Set[Symbol]()
        val params    = mutable.Set[Symbol]()
        val patvars   = mutable.Set[Symbol]()

        def defnSymbols = defnTrees.toList map (_.symbol)
        def localVars   = defnSymbols filter (t => t.isLocalToBlock && t.isVar)

        def qualifiesTerm(sym: Symbol) = (
             (sym.isModule || sym.isMethod || sym.isPrivateLocal || sym.isLocalToBlock)
          && !nme.isLocalName(sym.name)
          && !sym.isParameter
          && !sym.isParamAccessor       // could improve this, but it's a pain
          && !sym.isEarlyInitialized    // lots of false positives in the way these are encoded
          && !(sym.isGetter && sym.accessed.isEarlyInitialized)
        )
        def qualifiesType(sym: Symbol) = !sym.isDefinedInPackage
        def qualifies(sym: Symbol) = (
             (sym ne null)
          && (sym.isTerm && qualifiesTerm(sym) || sym.isType && qualifiesType(sym))
        )

        override def traverse(t: Tree): Unit = {
          val sym = t.symbol
          t match {
            case m: MemberDef if qualifies(t.symbol)   =>
              defnTrees += m
              t match {
                case DefDef(mods@_, name@_, tparams@_, vparamss, tpt@_, rhs@_) if !sym.isAbstract && !sym.isDeprecated && !sym.isMacro =>
                  if (sym.isPrimaryConstructor)
                    for (cpa <- sym.owner.constrParamAccessors if cpa.isPrivateLocal) params += cpa
                  else if (sym.isSynthetic && sym.isImplicit) return
                  else if (!sym.isConstructor)
                    for (vs <- vparamss) params ++= vs.map(_.symbol)
                case _ =>
              }
            case CaseDef(pat, guard@_, rhs@_) if settings.warnUnusedPatVars
                                                       => pat.foreach {
                                                            // TODO don't warn in isDefinedAt of $anonfun
                                                            case b @ Bind(n, _) if !atBounded(b) && n != nme.DEFAULT_CASE => patvars += b.symbol
                                                            case _ =>
                                                          }
            case _: RefTree if sym ne null             => targets += sym
            case Assign(lhs, _) if lhs.symbol != null  => setVars += lhs.symbol
            case Bind(_, _) if atBounded(t)            => atBounds += sym
            case _                                     =>
          }
          // Only record type references which don't originate within the
          // definition of the class being referenced.
          if (t.tpe ne null) {
            for (tp <- t.tpe if !treeTypes(tp) && !currentOwner.ownerChain.contains(tp.typeSymbol)) {
              tp match {
                case NoType | NoPrefix    =>
                case NullaryMethodType(_) =>
                case MethodType(_, _)     =>
                case SingleType(_, _)     =>
                case _                    =>
                  log(s"$tp referenced from $currentOwner")
                  treeTypes += tp
              }
            }
            // e.g. val a = new Foo ; new a.Bar ; don't let a be reported as unused.
            t.tpe.prefix foreach {
              case SingleType(_, sym) => targets += sym
              case _                 =>
            }
          }
          super.traverse(t)
        }
        def isUnusedType(m: Symbol): Boolean = (
              m.isType
          && !m.isTypeParameterOrSkolem // would be nice to improve this
          && (m.isPrivate || m.isLocalToBlock)
          && !(treeTypes.exists(tp => tp exists (t => t.typeSymbolDirect == m)))
        )
        def isSyntheticWarnable(sym: Symbol) = (
          sym.isDefaultGetter 
        )
        
        def isUnusedTerm(m: Symbol): Boolean = (
             m.isTerm
          && (!m.isSynthetic || isSyntheticWarnable(m))
          && ((m.isPrivate && !(m.isConstructor && m.owner.isAbstract)) || m.isLocalToBlock)
          && !targets(m)
          && !(m.name == nme.WILDCARD)              // e.g. val _ = foo
          && (m.isValueParameter || !ignoreNames(m.name.toTermName)) // serialization methods
          && !isConstantType(m.info.resultType)     // subject to constant inlining
          && !treeTypes.exists(_ contains m)        // e.g. val a = new Foo ; new a.Bar
          //&& !(m.isVal && m.info.resultType =:= typeOf[Unit])      // Unit val is uninteresting
        )
        def isUnusedParam(m: Symbol): Boolean = (
             isUnusedTerm(m)
          && !m.isDeprecated
          && !m.owner.isDefaultGetter
          && !(m.isParamAccessor && (
            m.owner.isImplicit ||
            targets.exists(s => s.isParameter
              && s.name == m.name && s.owner.isConstructor && s.owner.owner == m.owner) // exclude ctor params
          ))
        )
        def sympos(s: Symbol): Int =
          if (s.pos.isDefined) s.pos.point else if (s.isTerm) s.asTerm.referenced.pos.point else -1
        def treepos(t: Tree): Int =
          if (t.pos.isDefined) t.pos.point else sympos(t.symbol)

        def unusedTypes = defnTrees.toList.filter(t => isUnusedType(t.symbol)).sortBy(treepos)
        def unusedTerms = {
          val all = defnTrees.toList.filter(v => isUnusedTerm(v.symbol))

          // filter out setters if already warning for getter, indicated by position.
          // also documentary names in patterns.
          all.filterNot(v =>
              v.symbol.isSetter && all.exists(g => g.symbol.isGetter && g.symbol.pos.point == v.symbol.pos.point)
           || atBounds.exists(x => v.symbol.pos.point == x.pos.point)
          ).sortBy(treepos)
        }
        // local vars which are never set, except those already returned in unused
        def unsetVars = localVars.filter(v => !setVars(v) && !isUnusedTerm(v)).sortBy(sympos)
        def unusedParams = params.toList.filter(isUnusedParam).sortBy(sympos)
        def inDefinedAt(p: Symbol) = p.owner.isMethod && p.owner.name == nme.isDefinedAt && p.owner.owner.isAnonymousFunction
        def unusedPatVars = patvars.toList.filter(p => isUnusedTerm(p) && !inDefinedAt(p)).sortBy(sympos)
      }

      object skipMacroCall extends UnusedPrivates {
        override def qualifiesTerm(sym: Symbol): Boolean =
          super.qualifiesTerm(sym) && !sym.isMacro
      }
      object skipMacroExpansion extends UnusedPrivates {
        override def traverse(t: Tree): Unit =
          if (!hasMacroExpansionAttachment(t)) super.traverse(t)
      }
      object checkMacroExpandee extends UnusedPrivates {
        override def traverse(t: Tree): Unit =
          super.traverse(if (hasMacroExpansionAttachment(t)) macroExpandee(t) else t)
      }

      private def warningsEnabled: Boolean = {
        val ss = settings
        import ss._
        warnUnusedPatVars || warnUnusedPrivates || warnUnusedLocals || warnUnusedParams
      }

      def run(unusedPrivates: UnusedPrivates)(body: Tree): Unit = {
        unusedPrivates.traverse(body)

        if (settings.warnUnusedLocals || settings.warnUnusedPrivates) {
          for (defn: DefTree <- unusedPrivates.unusedTerms) {
            val sym = defn.symbol
            val pos = (
              if (defn.pos.isDefined) defn.pos
              else if (sym.pos.isDefined) sym.pos
              else sym match {
                case sym: TermSymbol => sym.referenced.pos
                case _               => NoPosition
              }
            )
            val why = if (sym.isPrivate) "private" else "local"
            val what = (
              if (sym.isDefaultGetter) "default argument"
              else if (sym.isConstructor) "constructor"
              else if (
                  sym.isVar
               || sym.isGetter && (sym.accessed.isVar || (sym.owner.isTrait && !sym.hasFlag(STABLE)))
              ) s"var ${sym.name.getterName.decoded}"
              else if (
                  sym.isVal
               || sym.isGetter && (sym.accessed.isVal || (sym.owner.isTrait && sym.hasFlag(STABLE)))
               || sym.isLazy
              ) s"val ${sym.name.decoded}"
              else if (sym.isSetter) s"setter of ${sym.name.getterName.decoded}"
              else if (sym.isMethod) s"method ${sym.name.decoded}"
              else if (sym.isModule) s"object ${sym.name.decoded}"
              else "term"
            )
            context.warning(pos, s"$why $what in ${sym.owner} is never used")
          }
          for (v <- unusedPrivates.unsetVars) {
            context.warning(v.pos, s"local var ${v.name} in ${v.owner} is never set: consider using immutable val")
          }
          for (t <- unusedPrivates.unusedTypes) {
            val sym = t.symbol
            val wrn = if (sym.isPrivate) settings.warnUnusedPrivates else settings.warnUnusedLocals
            if (wrn) {
              val why = if (sym.isPrivate) "private" else "local"
              context.warning(t.pos, s"$why ${sym.fullLocationString} is never used")
            }
          }
        }
        if (settings.warnUnusedPatVars) {
          for (v <- unusedPrivates.unusedPatVars)
            context.warning(v.pos, s"pattern var ${v.name} in ${v.owner} is never used; `${v.name}@_' suppresses this warning")
        }
        if (settings.warnUnusedParams) {
          def isImplementation(m: Symbol): Boolean = {
            def classOf(s: Symbol): Symbol = if (s.isClass || s == NoSymbol) s else classOf(s.owner)
            val opc = new overridingPairs.Cursor(classOf(m))
            opc.iterator.exists(pair => pair.low == m)
          }
          def isConvention(p: Symbol): Boolean = {
            (p.name.decoded == "args" && p.owner.isMethod && p.owner.name.decoded == "main") ||
            (p.tpe =:= typeOf[scala.Predef.DummyImplicit])
          }
          def warningIsOnFor(s: Symbol) = if (s.isImplicit) settings.warnUnusedImplicits else settings.warnUnusedExplicits
          def warnable(s: Symbol) = (
               warningIsOnFor(s)
            && !isImplementation(s.owner)
            && !isConvention(s)
          )
          for (s <- unusedPrivates.unusedParams if warnable(s))
            context.warning(s.pos, s"parameter $s in ${s.owner} is never used")
        }
      }
      def apply(unit: CompilationUnit): Unit = if (warningsEnabled && !unit.isJava) {
        val body = unit.body
        // TODO the message should distinguish whether the unusage is before or after macro expansion.
        settings.warnMacros.value match {
          case "none"   => run(skipMacroExpansion)(body)
          case "before" => run(checkMacroExpandee)(body)
          case "after"  => run(skipMacroCall)(body)
          case "both"   => run(checkMacroExpandee)(body) ; run(skipMacroCall)(body)
        }
      }
    }

    object checkDead {
      private val exprStack: mutable.Stack[Symbol] = mutable.Stack(NoSymbol)
      // The method being applied to `tree` when `apply` is called.
      private def expr = exprStack.top

      private def exprOK =
        (expr != Object_synchronized) &&
        !(expr.isLabel && treeInfo.isSynthCaseSymbol(expr)) // it's okay to jump to matchEnd (or another case) with an argument of type nothing

      private def treeOK(tree: Tree) = {
        val isLabelDef = tree match { case _: LabelDef => true; case _ => false}
        tree.tpe != null && tree.tpe.typeSymbol == NothingClass && !isLabelDef
      }

      @inline def updateExpr[A](fn: Tree)(f: => A) = {
        if (fn.symbol != null && fn.symbol.isMethod && !fn.symbol.isConstructor) {
          exprStack push fn.symbol
          try f finally exprStack.pop()
        } else f
      }
      def apply(tree: Tree): Tree = {
        if (settings.warnDeadCode && context.unit.exists && treeOK(tree) && exprOK)
          context.warning(tree.pos, "dead code following this construct")
        tree
      }

      // The checkDead call from typedArg is more selective.
      def inMode(mode: Mode, tree: Tree): Tree = if (mode.typingMonoExprByValue) apply(tree) else tree
    }

    private def symWasOverloaded(sym: Symbol) = sym.owner.isClass && sym.owner.info.member(sym.name).isOverloaded
    private def cyclicAdjective(sym: Symbol)  = if (symWasOverloaded(sym)) "overloaded" else "recursive"

    /** Returns Some(msg) if the given tree is untyped apparently due
     *  to a cyclic reference, and None otherwise.
     */
    def cyclicReferenceMessage(sym: Symbol, tree: Tree) = condOpt(tree) {
      case ValDef(_, _, TypeTree(), _)       => "recursive "+sym+" needs type"
      case DefDef(_, _, _, _, TypeTree(), _) => List(cyclicAdjective(sym), sym, "needs result type") mkString " "
      case Import(expr, selectors)           =>
        ( "encountered unrecoverable cycle resolving import." +
          "\nNote: this is often due in part to a class depending on a definition nested within its companion." +
          "\nIf applicable, you may wish to try moving some members into another object."
        )
    }

    // warn about class/method/type-members' type parameters that shadow types already in scope
    def warnTypeParameterShadow(tparams: List[TypeDef], sym: Symbol): Unit =
      if (settings.warnTypeParameterShadow && !isPastTyper && !sym.isSynthetic) {
        def enclClassOrMethodOrTypeMember(c: Context): Context =
          if (!c.owner.exists || c.owner.isClass || c.owner.isMethod || (c.owner.isType && !c.owner.isParameter)) c
          else enclClassOrMethodOrTypeMember(c.outer)

        tparams.filter(_.name != typeNames.WILDCARD).foreach { tp =>
        // we don't care about type params shadowing other type params in the same declaration
        enclClassOrMethodOrTypeMember(context).outer.lookupSymbol(tp.name, s => s != tp.symbol && s.hasRawInfo && reallyExists(s)) match {
          case LookupSucceeded(_, sym2) => context.warning(tp.pos,
            s"type parameter ${tp.name} defined in $sym shadows $sym2 defined in ${sym2.owner}. You may want to rename your type parameter, or possibly remove it.")
          case _ =>
        }
      }
    }

    /** Report a type error.
     *
     *  @param pos    The position where to report the error
     *  @param ex     The exception that caused the error
     */
    def reportTypeError(context0: Context, pos: Position, ex: TypeError) {
      if (ex.pos == NoPosition) ex.pos = pos
      // TODO: should be replaced by throwErrors
      // but it seems that throwErrors excludes some of the errors that should actually be
      // buffered, causing TypeErrors to fly around again. This needs some more investigation.
      if (!context0.reportErrors) throw ex
      if (settings.debug) ex.printStackTrace()

      ex match {
        case CyclicReference(sym, info: TypeCompleter) =>
          if (context0.owner.isTermMacro) {
            // see comments to TypeSigError for an explanation of this special case
            throw ex
          } else {
            val pos = info.tree match {
              case Import(expr, _)  => expr.pos
              case _                => ex.pos
            }
            context0.error(pos, cyclicReferenceMessage(sym, info.tree) getOrElse ex.getMessage())

            if (sym == ObjectClass)
              throw new FatalError("cannot redefine root "+sym)
          }
        case _ =>
          context0.error(ex.pos, ex.msg)
      }
    }
  }
}
