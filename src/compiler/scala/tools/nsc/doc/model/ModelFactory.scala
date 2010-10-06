/* NSC -- new Scala compiler -- Copyright 2007-2010 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
class ModelFactory(val global: Global, val settings: doc.Settings) { thisFactory: ModelFactory with CommentFactory with TreeFactory =>

  import global._
  import definitions.{ ObjectClass, ScalaObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyRefClass }

  private var droppedPackages = 0
  def templatesCount = templatesCache.size - droppedPackages

  private var modelFinished = false
  private var universe: Universe = null

  /**  */
  def makeModel: Universe = {
    val universe = new Universe { thisUniverse =>
      thisFactory.universe = thisUniverse
      val settings = thisFactory.settings
      val rootPackage =
        makeRootPackage getOrElse { throw new Error("no documentable class found in compilation units") }
    }
    modelFinished = true
    thisFactory.universe = null
    universe
  }

  /** */
  protected val templatesCache =
    new mutable.LinkedHashMap[Symbol, DocTemplateImpl]

  def findTemplate(query: String): Option[DocTemplateImpl] = {
    if (!modelFinished) throw new Error("cannot find template in unfinished universe")
    templatesCache.values find { tpl => tpl.qualifiedName == query && !tpl.isObject }
  }

  def optimize(str: String): String =
    if (str.length < 16) str.intern else str

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */

  /** Provides a default implementation for instances of the `Entity` type. */
  abstract class EntityImpl(val sym: Symbol, inTpl: => TemplateImpl) extends Entity {
    val name = optimize(sym.nameString)
    def inTemplate: TemplateImpl = inTpl
    def toRoot: List[EntityImpl] = this :: inTpl.toRoot
    def qualifiedName = name
    val universe = thisFactory.universe
  }

  /** Provides a default implementation for instances of the `WeakTemplateEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  trait TemplateImpl extends EntityImpl with TemplateEntity {
    override def qualifiedName: String =
      if (inTemplate.isRootPackage) name else optimize(inTemplate.qualifiedName + "." + name)
    def isPackage = sym.isPackage
    def isTrait = sym.isTrait
    def isClass = sym.isClass && !sym.isTrait
    def isObject = sym.isModule && !sym.isPackage
    def isCaseClass = sym.isClass && sym.hasFlag(Flags.CASE)
    def isRootPackage = false
    def selfType = if (sym.thisSym eq sym) None else Some(makeType(sym.thisSym.typeOfThis, this))
  }

  /** Provides a default implementation for instances of the `WeakTemplateEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  class NoDocTemplateImpl(sym: Symbol, inTpl: => TemplateImpl) extends EntityImpl(sym, inTpl) with TemplateImpl with NoDocTemplate {
    def isDocTemplate = false
  }

  /** Provides a default implementation for instances of the `MemberEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  abstract class MemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends EntityImpl(sym, inTpl) with MemberEntity {
    lazy val comment =
      if (inTpl == null) None else thisFactory.comment(sym, inTpl)
    override def inTemplate = inTpl
    override def toRoot: List[MemberImpl] = this :: inTpl.toRoot
    def inDefinitionTemplates =
      if (inTpl == null)
        makeRootPackage.toList
      else
        makeTemplate(sym.owner) :: (sym.allOverriddenSymbols map { inhSym => makeTemplate(inhSym.owner) })
    def visibility = {
      if (sym.isPrivateLocal) PrivateInInstance()
      else if (sym.isProtectedLocal) ProtectedInInstance()
      else {
        val qual =
          if (sym.privateWithin != null && sym.privateWithin != NoSymbol)
            Some(makeTemplate(sym.privateWithin))
          else None
        if (sym.isPrivate) PrivateInTemplate(inTpl)
        else if (sym.isProtected) ProtectedInTemplate(qual getOrElse inTpl)
        else if (qual.isDefined) PrivateInTemplate(qual.get)
        else Public()
      }
    }
    def flags = {
      val fgs = mutable.ListBuffer.empty[Paragraph]
      if (sym.isImplicit) fgs += Paragraph(Text("implicit"))
      if (sym hasFlag Flags.SEALED) fgs += Paragraph(Text("sealed"))
      if (!sym.isTrait && (sym hasFlag Flags.ABSTRACT)) fgs += Paragraph(Text("abstract"))
      if (!sym.isTrait && (sym hasFlag Flags.DEFERRED)) fgs += Paragraph(Text("abstract"))
      if (!sym.isModule && (sym hasFlag Flags.FINAL)) fgs += Paragraph(Text("final"))
      fgs.toList
    }
    def deprecation =
      if (sym.isDeprecated)
        Some(sym.deprecationMessage match {
          case Some(msg) => parseWiki(msg, NoPosition)
          case None =>Body(Nil)
        })
      else
        comment flatMap { _.deprecated }
    def inheritedFrom =
      if (inTemplate.sym == this.sym.owner || inTemplate.sym.isPackage) Nil else
        makeTemplate(this.sym.owner) :: (sym.allOverriddenSymbols map { os => makeTemplate(os.owner) })
    def resultType = {
      def resultTpe(tpe: Type): Type = tpe match { // similar to finalResultType, except that it leaves singleton types alone
        case PolyType(_, res) => resultTpe(res)
        case MethodType(_, res) => resultTpe(res)
        case _ => tpe
      }
      makeType(resultTpe(sym.tpe), inTemplate, sym)
    }
    def isDef = false
    def isVal = false
    def isLazyVal = false
    def isVar = false
    def isImplicit = sym.isImplicit
    def isConstructor = false
    def isAliasType = false
    def isAbstractType = false
    def isAbstract =
      ((!sym.isTrait && ((sym hasFlag Flags.ABSTRACT) || (sym hasFlag Flags.DEFERRED))) ||
      sym.isAbstractClass || sym.isAbstractType) && !sym.isSynthetic
    def isTemplate = false
  }

  /** Provides a default implementation for instances of the `TemplateEntity` type. It must be instantiated as a
    * `TemplateSymbolicEntity` to access the compiler symbol that underlies the entity and to be registered with the
    * `templatesCache` at the very start of its instantiation.
    *
    * The instantiation of `TemplateImpl` triggers the creation of the following entities.
    * * The owner of the template (as a full template);
    * * All ancestors of the template (as weak templates);
    * * All non-package members (including other templates, as full templates). */
  abstract class DocTemplateImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends MemberImpl(sym, inTpl) with TemplateImpl with HigherKindedImpl with DocTemplateEntity {
    //if (inTpl != null) println("mbr " + sym + " in " + (inTpl.toRoot map (_.sym)).mkString(" > "))
    templatesCache += (sym -> this)
    lazy val definitionName = optimize(inDefinitionTemplates.head.qualifiedName + "." + name)
    override def toRoot: List[DocTemplateImpl] = this :: inTpl.toRoot
    def inSource = if (sym.sourceFile != null) Some(sym.sourceFile, sym.pos.line) else None
    def sourceUrl = {
      def fixPath(s: String) = s.replaceAll("\\" + java.io.File.separator, "/")
      val assumedSourceRoot: String = {
        val fixed = fixPath(settings.sourcepath.value)
        if (fixed endsWith "/") fixed.dropRight(1) else fixed
      }
      if (!settings.docsourceurl.isDefault)
        inSource map { case (file, _) =>
          val filePath = fixPath(file.path).replaceFirst("^" + assumedSourceRoot, "").stripSuffix(".scala")
          val tplOwner = this.inTemplate.qualifiedName
          val tplName = this.name
          val patches = new Regex("""€\{(FILE_PATH|TPL_OWNER|TPL_NAME)\}""")
          val patchedString = patches.replaceAllIn(settings.docsourceurl.value, { m => m.group(1) match {
              case "FILE_PATH" => filePath
              case "TPL_OWNER" => tplOwner
              case "TPL_NAME" => tplName
            }
          })
          new java.net.URL(patchedString)
        }
      else None
    }
    def parentType = {
      if (sym.isPackage) None else {
        val tps =
          (sym.tpe.parents filter (_ != ScalaObjectClass.tpe)) map { _.asSeenFrom(sym.thisType, sym) }
        Some(makeType(RefinedType(tps, EmptyScope), inTpl))
      }
    }
    val linearization: List[(TemplateEntity, TypeEntity)] = {
      val acs = sym.ancestors filter { _ != ScalaObjectClass }
      val tps = acs map { cls => makeType(sym.info.baseType(cls), this) }
      val tpls = acs map { makeTemplate(_) }
      tpls map {
          case dtpl: DocTemplateImpl => dtpl.registerSubClass(this)
          case _ =>
      }
      tpls zip tps
    }
    def linearizationTemplates = linearization map { _._1 }
    def linearizationTypes = linearization map { _._2 }
    private lazy val subClassesCache = mutable.Buffer.empty[DocTemplateEntity]
    def registerSubClass(sc: DocTemplateEntity): Unit = {
      assert(subClassesCache != null)
      subClassesCache += sc
    }
    def subClasses = subClassesCache.toList
    protected lazy val memberSyms =
       // Only this class's constructors are part of its members, inherited constructors are not.
      sym.info.members.filter(s => localShouldDocument(s) && (!s.isConstructor || s.owner == sym))
    val members       = memberSyms flatMap (makeMember(_, this))
    val templates     = members collect { case c: DocTemplateEntity => c }
    val methods       = members collect { case d: Def => d }
    val values        = members collect { case v: Val => v }
    val abstractTypes = members collect { case t: AbstractType => t }
    val aliasTypes    = members collect { case t: AliasType => t }
    override def isTemplate = true
    def isDocTemplate = true
    def companion = sym.companionSymbol match {
      case NoSymbol => None
      case comSym if !isEmptyJavaObject(comSym) => Some(makeDocTemplate(comSym, inTpl))
      case _ => None
    }
  }

  abstract class PackageImpl(sym: Symbol, inTpl: => PackageImpl) extends DocTemplateImpl(sym, inTpl) with Package {
    override def inTemplate = inTpl
    override def toRoot: List[PackageImpl] = this :: inTpl.toRoot
    val packages = members collect { case p: Package => p }
  }

  abstract class RootPackageImpl(sym: Symbol) extends PackageImpl(sym, null) with RootPackageEntity

  abstract class NonTemplateMemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends MemberImpl(sym, inTpl) with NonTemplateMemberEntity {
    override def qualifiedName = optimize(inTemplate.qualifiedName + "#" + name)
    lazy val definitionName = optimize(inDefinitionTemplates.head.qualifiedName + "#" + name)
    def isUseCase = sym.isSynthetic
  }

  abstract class NonTemplateParamMemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends NonTemplateMemberImpl(sym, inTpl) {
    def valueParams =
      sym.paramss map { ps => (ps.zipWithIndex) map { case (p, i) =>
        if (p.nameString contains "$") makeValueParam(p, inTpl, optimize("arg" + i)) else makeValueParam(p, inTpl)
      }}
  }

  abstract class ParameterImpl(sym: Symbol, inTpl: => TemplateImpl) extends EntityImpl(sym, inTpl) with ParameterEntity {
    override def inTemplate = inTpl
  }

  private trait TypeBoundsImpl extends EntityImpl {
    def lo = sym.info.bounds match {
      case TypeBounds(lo, hi) if lo.typeSymbol != NothingClass =>
        Some(makeType(appliedType(lo, sym.info.typeParams map {_.tpe}), inTemplate))
      case _ => None
    }
    def hi = sym.info.bounds match {
      case TypeBounds(lo, hi) if hi.typeSymbol != AnyClass =>
        Some(makeType(appliedType(hi, sym.info.typeParams map {_.tpe}), inTemplate))
      case _ => None
    }
  }

  trait HigherKindedImpl extends EntityImpl with HigherKinded {
    def typeParams =
      sym.typeParams map (makeTypeParam(_, inTemplate))
  }

  /* ============== MAKER METHODS ============== */

  /** */
  def normalizeTemplate(aSym: Symbol): Symbol = aSym match {
    case null | EmptyPackage | NoSymbol =>
      normalizeTemplate(RootPackage)
    case ScalaObjectClass | ObjectClass =>
      normalizeTemplate(AnyRefClass)
    case _ if aSym.isModuleClass || aSym.isPackageObject =>
      normalizeTemplate(aSym.sourceModule)
    case _ =>
      aSym
  }

  def makeRootPackage: Option[PackageImpl] =
    makePackage(RootPackage, null)

  /** Creates a package entity for the given symbol or returns `None` if the symbol does not denote a package that
    * contains at least one ''documentable'' class, trait or object. Creating a package entity */
  def makePackage(aSym: Symbol, inTpl: => PackageImpl): Option[PackageImpl] = {
    val bSym = normalizeTemplate(aSym)
    if (templatesCache isDefinedAt (bSym))
      Some(templatesCache(bSym) match {case p: PackageImpl => p})
    else {
      val pack =
        if (bSym == RootPackage)
          new RootPackageImpl(bSym) {
            override val name = "root"
            override def inTemplate = this
            override def toRoot = this :: Nil
            override def qualifiedName = "_root_"
            override def inheritedFrom = Nil
            override def isRootPackage = true
            override protected lazy val memberSyms =
              (bSym.info.members ++ EmptyPackage.info.members) filter { s =>
                s != EmptyPackage && s != RootPackage
              }
          }
        else
          new PackageImpl(bSym, inTpl) {}
      if (pack.templates.isEmpty) {
        droppedPackages += 1
        None
      }
      else Some(pack)
    }

  }

  /** */
  def makeTemplate(aSym: Symbol): TemplateImpl = {
    val bSym = normalizeTemplate(aSym)
    if (bSym == RootPackage)
      makeRootPackage.get
    else if (bSym.isPackage)
      makeTemplate(bSym.owner) match {
        case inPkg: PackageImpl => makePackage(bSym, inPkg) getOrElse (new NoDocTemplateImpl(bSym, inPkg))
        case _ => throw new Error("'" + bSym + "' must be in a package")
      }
    else if (templateShouldDocument(bSym))
      makeTemplate(bSym.owner) match {
        case inDTpl: DocTemplateImpl => makeDocTemplate(bSym, inDTpl)
        case _ => throw new Error("'" + bSym + "' must be in documentable template")
      }
    else
      new NoDocTemplateImpl(bSym, makeTemplate(bSym.owner))
  }

  /** */
  def makeDocTemplate(aSym: Symbol, inTpl: => DocTemplateImpl): DocTemplateImpl = {
    val bSym = normalizeTemplate(aSym)
    val minimumInTpl =
      if (bSym.owner != inTpl.sym)
        makeTemplate(aSym.owner) match {
          case inDTpl: DocTemplateImpl => inDTpl
          case inNDTpl => throw new Error("'" + bSym + "' is owned by '" + inNDTpl + "' which is not documented")
        }
      else
        inTpl
    if (templatesCache isDefinedAt (bSym))
      templatesCache(bSym)
    else if (bSym.isModule || (bSym.isAliasType && bSym.tpe.typeSymbol.isModule))
      new DocTemplateImpl(bSym, minimumInTpl) with Object
    else if (bSym.isTrait || (bSym.isAliasType && bSym.tpe.typeSymbol.isTrait))
      new DocTemplateImpl(bSym, minimumInTpl) with Trait
    else if (bSym.isClass || (bSym.isAliasType && bSym.tpe.typeSymbol.isClass))
      new DocTemplateImpl(bSym, minimumInTpl) with Class {
        def valueParams =
          // we don't want params on a class (non case class) signature
          if (isCaseClass) List(sym.constrParamAccessors map (makeValueParam(_, this)))
          else List.empty
        val constructors =
          members collect { case d: Constructor => d }
        def primaryConstructor = constructors find { _.isPrimary }
      }
    else if (isNestedObjectLazyVal(bSym)) {
      new DocTemplateImpl(bSym, minimumInTpl) with Object {
        override def isObject = true
        override def isLazyVal = false
      }
    } else
      throw new Error("'" + bSym + "' that isn't a class, trait or object cannot be built as a documentable template")
  }

  /** */
  def makeMember(aSym: Symbol, inTpl: => DocTemplateImpl): List[MemberImpl] = {

    def makeMember0(bSym: Symbol): Option[MemberImpl] = {
      if (bSym.isGetter && bSym.isLazy)
        if (isNestedObjectLazyVal(bSym))
          if (templateShouldDocument(bSym))
            Some(makeDocTemplate(bSym, inTpl))
          else None
        else
          Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
                override def isLazyVal = true
              })
      else if (bSym.isGetter && bSym.accessed.isMutable)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
          override def isVar = true
        })
      else if (bSym.isMethod && !bSym.isGetterOrSetter && !bSym.isConstructor && !bSym.isModule)
        Some(new NonTemplateParamMemberImpl(bSym, inTpl) with HigherKindedImpl with Def {
          override def isDef = true
        })
      else if (bSym.isConstructor)
        Some(new NonTemplateParamMemberImpl(bSym, inTpl) with Constructor {
          override def isConstructor = true
          def isPrimary = sym.isPrimaryConstructor
        })
      else if (bSym.isGetter) // Scala field accessor or Java field
        Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
          override def isVal = true
        })
      else if (bSym.isAbstractType)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with TypeBoundsImpl with HigherKindedImpl with AbstractType {
          override def isAbstractType = true
        })
      else if (bSym.isAliasType)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with HigherKindedImpl with AliasType {
          override def isAliasType = true
          def alias = makeType(sym.tpe.dealias, inTpl, sym)
        })
      else if (bSym.isPackage)
        inTpl match { case inPkg: PackageImpl =>  makePackage(bSym, inPkg) }
      else if ((bSym.isClass || bSym.isModule) && templateShouldDocument(bSym))
        Some(makeDocTemplate(bSym, inTpl))
      else
        None
    }

    if (!localShouldDocument(aSym) || aSym.isModuleClass || aSym.isPackageObject || aSym.isMixinConstructor)
      Nil
    else {
      val allSyms = useCases(aSym, inTpl.sym) map { case (bSym, bComment, bPos) =>
        addCommentBody(bSym, inTpl, bComment, bPos)
      }
      (allSyms :+ aSym) flatMap { makeMember0(_) }
    }

  }

  /** */
  def makeTypeParam(aSym: Symbol, inTpl: => TemplateImpl): TypeParam =
    new ParameterImpl(aSym, inTpl) with TypeBoundsImpl with HigherKindedImpl with TypeParam {
      def isTypeParam = true
      def isValueParam = false
      def variance: String = {
        if (sym hasFlag Flags.COVARIANT) "+"
        else if (sym hasFlag Flags.CONTRAVARIANT) "-"
        else ""
      }
    }

  /** */
  def makeValueParam(aSym: Symbol, inTpl: => DocTemplateImpl): ValueParam = {
    makeValueParam(aSym, inTpl, aSym.nameString)
  }

  /** */
  def makeValueParam(aSym: Symbol, inTpl: => DocTemplateImpl, newName: String): ValueParam =
    new ParameterImpl(aSym, inTpl) with ValueParam {
      override val name = newName
      def isTypeParam = false
      def isValueParam = true
      def defaultValue =
        if (aSym.hasDefault)
          // units.filter should return only one element
          (currentRun.units filter (_.source.file == aSym.sourceFile)).toList match {
            case List(unit) =>
              (unit.body find (_.symbol == aSym)) match {
                case Some(ValDef(_,_,_,rhs)) =>
                  Some(makeTree(rhs))
                case _ => None
              }
            case _ => None
          }
        else None
      def resultType =
        makeType(sym.tpe, inTpl, sym)
      def isImplicit = aSym.isImplicit
    }

  /** */
  def makeType(aType: Type, inTpl: => TemplateImpl, dclSym: Symbol): TypeEntity = {
    def ownerTpl(sym: Symbol): Symbol =
      if (sym.isClass || sym.isModule || sym == NoSymbol) sym else ownerTpl(sym.owner)
    val tpe =
      if (thisFactory.settings.useStupidTypes.value) aType else {
        def ownerTpl(sym: Symbol): Symbol =
          if (sym.isClass || sym.isModule || sym == NoSymbol) sym else ownerTpl(sym.owner)
        val fixedSym = if (inTpl.sym.isModule) inTpl.sym.moduleClass else inTpl.sym
        aType.asSeenFrom(fixedSym.thisType, ownerTpl(dclSym))
      }
    makeType(tpe, inTpl)
  }

  /** */
  def makeType(aType: Type, inTpl: => TemplateImpl): TypeEntity =
    new TypeEntity {
      private val nameBuffer = new StringBuilder
      private var refBuffer = new immutable.TreeMap[Int, (TemplateEntity, Int)]
      private def appendTypes0(types: List[Type], sep: String): Unit = types match {
        case Nil =>
        case tp :: Nil =>
          appendType0(tp)
        case tp :: tps =>
          appendType0(tp)
          nameBuffer append sep
          appendTypes0(tps, sep)
      }
      private def checkFunctionType(tpe: TypeRef): Boolean = {
        val TypeRef(_, sym, args) = tpe
        (args.length > 0) && (args.length - 1 <= definitions.MaxFunctionArity) &&
        (sym == definitions.FunctionClass(args.length - 1))
      }
      private def appendType0(tpe: Type): Unit = tpe match {
        /* Type refs */
        case tp: TypeRef if (checkFunctionType(tp)) =>
          nameBuffer append '('
          appendTypes0(tp.args.init, ", ")
          nameBuffer append ") ⇒ "
          appendType0(tp.args.last)
        case tp: TypeRef if (tp.typeSymbol == definitions.RepeatedParamClass) =>
          appendType0(tp.args.head)
          nameBuffer append '*'
        case tp: TypeRef if (tp.typeSymbol == definitions.ByNameParamClass) =>
          nameBuffer append "⇒ "
          appendType0(tp.args.head)
        case tp: TypeRef if (definitions.isTupleTypeOrSubtype(tp)) =>
          nameBuffer append '('
          appendTypes0(tp.args, ", ")
          nameBuffer append ')'
        case TypeRef(pre, aSym, targs) =>
          val bSym = normalizeTemplate(aSym)
          if (bSym.isNonClassType)
            nameBuffer append bSym.name
          else {
            val tpl = makeTemplate(bSym)
            val pos0 = nameBuffer.length
            refBuffer += pos0 -> (tpl, tpl.name.length)
            nameBuffer append tpl.name
          }
          if (!targs.isEmpty) {
            nameBuffer append '['
            appendTypes0(targs, ", ")
            nameBuffer append ']'
          }
        /* Refined types */
        case RefinedType(parents, defs) =>
          appendTypes0((if (parents.length > 1) parents filterNot (_ == ObjectClass.tpe) else parents), " with ")
          if (!defs.isEmpty) {
            nameBuffer append " {...}" // TODO: actually print the refinement
          }
        /* Polymorphic types */
        case PolyType(tparams, result) if tparams nonEmpty =>
//          throw new Error("Polymorphic type '" + tpe + "' cannot be printed as a type")
          def typeParamsToString(tps: List[Symbol]): String = if(tps isEmpty) "" else
            tps.map{tparam =>
              tparam.varianceString + tparam.name + typeParamsToString(tparam.typeParams)
            }.mkString("[", ", ", "]")
          nameBuffer append typeParamsToString(tparams)
          appendType0(result)
        case PolyType(tparams, result) if (tparams.isEmpty) =>
          nameBuffer append '⇒'
          appendType0(result)
        case tpen =>
          nameBuffer append tpen.toString
      }
      appendType0(aType)
      val refEntity = refBuffer
      val name = optimize(nameBuffer.toString)
    }

  def templateShouldDocument(aSym: Symbol): Boolean = {
  	// TODO: document sourceless entities (e.g., Any, etc), based on a new Setting to be added
  	(aSym.isPackageClass || (aSym.sourceFile != null)) && localShouldDocument(aSym) &&
    ( aSym.owner == NoSymbol || templateShouldDocument(aSym.owner) ) && !isEmptyJavaObject(aSym)
  }

  def isNestedObjectLazyVal(aSym: Symbol): Boolean = {
    aSym.isLazy && !aSym.isRootPackage && !aSym.owner.isPackageClass && (aSym.lazyAccessor != NoSymbol)
  }

  def isEmptyJavaObject(aSym: Symbol): Boolean = {
    def hasMembers = aSym.info.members.exists(s => localShouldDocument(s) && (!s.isConstructor || s.owner == aSym))
    aSym.isModule && aSym.hasFlag(Flags.JAVA) && !hasMembers
  }

  def localShouldDocument(aSym: Symbol): Boolean = {
    !aSym.isPrivate && (aSym.isProtected || aSym.privateWithin == NoSymbol) && (!aSym.isSynthetic || isNestedObjectLazyVal(aSym))
  }
}
