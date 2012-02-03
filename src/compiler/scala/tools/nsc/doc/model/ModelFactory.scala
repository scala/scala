/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags

import io._

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
class ModelFactory(val global: Global, val settings: doc.Settings) {
  thisFactory: ModelFactory with CommentFactory with TreeFactory =>

  import global._
  import definitions.{ ObjectClass, RootPackage, EmptyPackage, NothingClass, AnyClass, AnyValClass, AnyRefClass }

  private var droppedPackages = 0
  def templatesCount = templatesCache.size - droppedPackages

  private var modelFinished = false
  private var universe: Universe = null

  private def dbg(msg: String) = if (sys.props contains "scala.scaladoc.debug") println(msg)
  private def closestPackage(sym: Symbol) = {
    if (sym.isPackage || sym.isPackageClass) sym
    else sym.enclosingPackage
  }

  private def printWithoutPrefix(memberSym: Symbol, templateSym: Symbol) = {
    dbg(
      "memberSym " + memberSym + " templateSym " + templateSym + " encls = " +
      closestPackage(memberSym) + ", " + closestPackage(templateSym)
    )
    memberSym.isOmittablePrefix || (closestPackage(memberSym) == closestPackage(templateSym))
  }

  private lazy val noSubclassCache = Set(AnyClass, AnyRefClass, ObjectClass)

  /**  */
  def makeModel: Option[Universe] = {
    val universe = new Universe { thisUniverse =>
      thisFactory.universe = thisUniverse
      val settings = thisFactory.settings
      private val rootPackageMaybe = makeRootPackage
      val rootPackage = rootPackageMaybe.orNull
    }
    modelFinished = true
    Some(universe) filter (_.rootPackage != null)
  }

  /** */
  protected val templatesCache =
    new mutable.LinkedHashMap[Symbol, DocTemplateImpl]

  def findTemplate(query: String): Option[DocTemplateImpl] = {
    if (!modelFinished) sys.error("cannot find template in unfinished universe")
    templatesCache.values find { tpl => tpl.qualifiedName == query && !tpl.isObject }
  }

  def optimize(str: String): String =
    if (str.length < 16) str.intern else str

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */

  abstract class EntityImpl(val sym: Symbol, inTpl: => TemplateImpl) extends Entity {
    val name = optimize(sym.nameString)
    def inTemplate: TemplateImpl = inTpl
    def toRoot: List[EntityImpl] = this :: inTpl.toRoot
    def qualifiedName = name
    val universe = thisFactory.universe
    def annotations = sym.annotations.map(makeAnnotation)
  }

  trait TemplateImpl extends EntityImpl with TemplateEntity {
    override def qualifiedName: String =
      if (inTemplate.isRootPackage) name else optimize(inTemplate.qualifiedName + "." + name)
    def isPackage = sym.isPackage
    def isTrait = sym.isTrait
    def isClass = sym.isClass && !sym.isTrait
    def isObject = sym.isModule && !sym.isPackage
    def isCaseClass = sym.isCaseClass
    def isRootPackage = false
    def selfType = if (sym.thisSym eq sym) None else Some(makeType(sym.thisSym.typeOfThis, this))
  }

  class NoDocTemplateImpl(sym: Symbol, inTpl: => TemplateImpl) extends EntityImpl(sym, inTpl) with TemplateImpl with NoDocTemplate {
    def isDocTemplate = false
  }

  abstract class MemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends EntityImpl(sym, inTpl) with MemberEntity {
    lazy val comment =
      if (inTpl == null) None else thisFactory.comment(sym, inTpl)
    override def inTemplate = inTpl
    override def toRoot: List[MemberImpl] = this :: inTpl.toRoot
    def inDefinitionTemplates = this match {
        case mb: NonTemplateMemberEntity if (mb.useCaseOf.isDefined) =>
          mb.useCaseOf.get.inDefinitionTemplates
        case _ =>
          if (inTpl == null) 
            makeRootPackage.toList
          else
            makeTemplate(sym.owner) :: (sym.allOverriddenSymbols map { inhSym => makeTemplate(inhSym.owner) })
      }
    def visibility = {
      if (sym.isPrivateLocal) PrivateInInstance()
      else if (sym.isProtectedLocal) ProtectedInInstance()
      else {
        val qual =
          if (sym.hasAccessBoundary)
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
      if (sym.isSealed) fgs += Paragraph(Text("sealed"))
      if (!sym.isTrait && (sym hasFlag Flags.ABSTRACT)) fgs += Paragraph(Text("abstract"))
      if (!sym.isTrait && (sym hasFlag Flags.DEFERRED)) fgs += Paragraph(Text("abstract"))
      if (!sym.isModule && (sym hasFlag Flags.FINAL)) fgs += Paragraph(Text("final"))
      fgs.toList    	
    }
    def deprecation =
      if (sym.isDeprecated)
        Some((sym.deprecationMessage, sym.deprecationVersion) match {
          case (Some(msg), Some(ver)) => parseWiki("''(Since version " + ver + ")'' " + msg, NoPosition)
          case (Some(msg), None) => parseWiki(msg, NoPosition)
          case (None, Some(ver)) =>  parseWiki("''(Since version " + ver + ")''", NoPosition)
          case (None, None) => Body(Nil)
        })
      else
        comment flatMap { _.deprecated }
    def migration =
      if(sym.hasMigrationAnnotation)
        Some((sym.migrationMessage, sym.migrationVersion) match {
          case (Some(msg), Some(ver)) => parseWiki("''(Changed in version " + ver + ")'' " + msg, NoPosition)
          case (Some(msg), None) => parseWiki(msg, NoPosition)
          case (None, Some(ver)) =>  parseWiki("''(Changed in version " + ver + ")''", NoPosition)
          case (None, None) => Body(Nil)
        })
      else
        None
    def inheritedFrom =
      if (inTemplate.sym == this.sym.owner || inTemplate.sym.isPackage) Nil else
        makeTemplate(this.sym.owner) :: (sym.allOverriddenSymbols map { os => makeTemplate(os.owner) })
    def resultType = {
      def resultTpe(tpe: Type): Type = tpe match { // similar to finalResultType, except that it leaves singleton types alone
        case PolyType(_, res) => resultTpe(res)
        case MethodType(_, res) => resultTpe(res)
        case NullaryMethodType(res) => resultTpe(res)
        case _ => tpe
      }
      makeTypeInTemplateContext(resultTpe(sym.tpe), inTemplate, sym)
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

   /** The instantiation of `TemplateImpl` triggers the creation of the following entities:
    *  All ancestors of the template and all non-package members.
    */
  abstract class DocTemplateImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends MemberImpl(sym, inTpl) with TemplateImpl with HigherKindedImpl with DocTemplateEntity {
    //if (inTpl != null) println("mbr " + sym + " in " + (inTpl.toRoot map (_.sym)).mkString(" > "))
    if (settings.verbose.value)
      inform("Creating doc template for " + sym)

    templatesCache += (sym -> this)
    lazy val definitionName = optimize(inDefinitionTemplates.head.qualifiedName + "." + name)
    override def toRoot: List[DocTemplateImpl] = this :: inTpl.toRoot
    def inSource =
      if (sym.sourceFile != null && ! sym.isSynthetic)
        Some((sym.sourceFile, sym.pos.line))
      else
        None

    def sourceUrl = {
      def fixPath(s: String) = s.replaceAll("\\" + java.io.File.separator, "/")
      val assumedSourceRoot  = fixPath(settings.sourcepath.value) stripSuffix "/"

      if (!settings.docsourceurl.isDefault)
        inSource map { case (file, _) =>
          val filePath = fixPath(file.path).replaceFirst("^" + assumedSourceRoot, "").stripSuffix(".scala")
          val tplOwner = this.inTemplate.qualifiedName
          val tplName = this.name
          val patches = new Regex("""€\{(FILE_PATH|TPL_OWNER|TPL_NAME)\}""")
          def substitute(name: String): String = name match {
            case "FILE_PATH" => filePath
            case "TPL_OWNER" => tplOwner
            case "TPL_NAME" => tplName
          }
          val patchedString = patches.replaceAllIn(settings.docsourceurl.value, m => java.util.regex.Matcher.quoteReplacement(substitute(m.group(1))) )
          new java.net.URL(patchedString)
        }
      else None
    }
    def parentType = {
      if (sym.isPackage || sym == AnyClass) None else {
        val tps = sym.tpe.parents map { _.asSeenFrom(sym.thisType, sym) }
        Some(makeType(RefinedType(tps, EmptyScope), inTpl))
      }
    }
    val linearization: List[(TemplateEntity, TypeEntity)] = {
      sym.ancestors map { ancestor =>
        val typeEntity = makeType(sym.info.baseType(ancestor), this)
        val tmplEntity = makeTemplate(ancestor) match {
          case tmpl: DocTemplateImpl  => tmpl registerSubClass this ; tmpl
          case tmpl                   => tmpl
        }
        (tmplEntity, typeEntity)
      }
    }

    def linearizationTemplates = linearization map { _._1 }
    def linearizationTypes = linearization map { _._2 }

    private lazy val subClassesCache = (
      if (noSubclassCache(sym)) null
      else mutable.ListBuffer[DocTemplateEntity]()
    )
    def registerSubClass(sc: DocTemplateEntity): Unit = {
      if (subClassesCache != null)
        subClassesCache += sc
    }
    def subClasses = if (subClassesCache == null) Nil else subClassesCache.toList

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
      case comSym if !isEmptyJavaObject(comSym) && (comSym.isClass || comSym.isModule) =>
        Some(makeDocTemplate(comSym, inTpl))
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
    def isBridge = sym.isBridge
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
        Some(makeTypeInTemplateContext(appliedType(lo, sym.info.typeParams map {_.tpe}), inTemplate, sym))
      case _ => None
    }
    def hi = sym.info.bounds match {
      case TypeBounds(lo, hi) if hi.typeSymbol != AnyClass =>
        Some(makeTypeInTemplateContext(appliedType(hi, sym.info.typeParams map {_.tpe}), inTemplate, sym))
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
    case ObjectClass =>
      normalizeTemplate(AnyRefClass)
    case _ if aSym.isPackageObject =>
      aSym
    case _ if aSym.isModuleClass =>
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
            override lazy val comment =
              if(settings.docRootContent.isDefault) None
              else {
                import Streamable._
                Path(settings.docRootContent.value) match {
                  case f : File => {
                    val rootComment = closing(f.inputStream)(is => parse(slurp(is), "", NoPosition))
                    Some(rootComment)
                  }
                  case _ => None
                }
              }
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
    else
      throw new Error("'" + bSym + "' that isn't a class, trait or object cannot be built as a documentable template")
  }

  /** */
  def makeAnnotation(annot: AnnotationInfo): Annotation = {
    val aSym = annot.symbol
    new EntityImpl(aSym, makeTemplate(aSym.owner)) with Annotation {
      lazy val annotationClass =
        makeTemplate(annot.symbol)
      val arguments = { // lazy
        def noParams = annot.args map { _ => None }
        val params: List[Option[ValueParam]] = annotationClass match {
          case aClass: Class =>
            (aClass.primaryConstructor map { _.valueParams.head }) match {
              case Some(vps) => vps map { Some(_) }
              case None => noParams
            }
          case _ => noParams
        }
        assert(params.length == annot.args.length)
        (params zip annot.args) flatMap { case (param, arg) =>
          makeTree(arg) match {
            case Some(tree) =>
              Some(new ValueArgument {
                def parameter = param
                def value = tree
              })
            case None => None
          }
        }
      }
    }
  }

  /** */
  def makeMember(aSym: Symbol, inTpl: => DocTemplateImpl): List[MemberImpl] = {

    def makeMember0(bSym: Symbol, _useCaseOf: Option[MemberImpl]): Option[MemberImpl] = {
      if (bSym.isGetter && bSym.isLazy)
          Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
            override lazy val comment = // The analyser does not duplicate the lazy val's DocDef when it introduces its accessor.
              thisFactory.comment(bSym.accessed, inTpl) // This hack should be removed after analyser is fixed.
            override def isLazyVal = true
            override def useCaseOf = _useCaseOf
          })
      else if (bSym.isGetter && bSym.accessed.isMutable)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
          override def isVar = true
          override def useCaseOf = _useCaseOf
        })
      else if (bSym.isMethod && !bSym.hasAccessorFlag && !bSym.isConstructor && !bSym.isModule) {
        val cSym = { // This unsightly hack closes issue #4086.
          if (bSym == definitions.Object_synchronized) {
            val cSymInfo = (bSym.info: @unchecked) match {
              case PolyType(ts, MethodType(List(bp), mt)) =>
                val cp = bp.cloneSymbol.setInfo(appliedType(definitions.ByNameParamClass.typeConstructor, List(bp.info)))
                PolyType(ts, MethodType(List(cp), mt))
            }
            bSym.cloneSymbol.setInfo(cSymInfo)
          }
          else bSym
        }
        Some(new NonTemplateParamMemberImpl(cSym, inTpl) with HigherKindedImpl with Def {
          override def isDef = true
          override def useCaseOf = _useCaseOf
        })
      }
      else if (bSym.isConstructor)
        Some(new NonTemplateParamMemberImpl(bSym, inTpl) with Constructor {
          override def isConstructor = true
          def isPrimary = sym.isPrimaryConstructor
          override def useCaseOf = _useCaseOf
        })
      else if (bSym.isGetter) // Scala field accessor or Java field
        Some(new NonTemplateMemberImpl(bSym, inTpl) with Val {
          override def isVal = true
          override def useCaseOf = _useCaseOf
        })
      else if (bSym.isAbstractType)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with TypeBoundsImpl with HigherKindedImpl with AbstractType {
          override def isAbstractType = true
          override def useCaseOf = _useCaseOf
        })
      else if (bSym.isAliasType)
        Some(new NonTemplateMemberImpl(bSym, inTpl) with HigherKindedImpl with AliasType {
          override def isAliasType = true
          def alias = makeTypeInTemplateContext(sym.tpe.dealias, inTpl, sym)
          override def useCaseOf = _useCaseOf
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
      	docComments.put(bSym, DocComment(bComment, bPos)) // put the comment in the list, don't parse it yet, closes SI-4898
        bSym
      }

      val member = makeMember0(aSym, None)
  		if (allSyms.isEmpty)
  			member.toList
    	else
    		// Use cases replace the original definitions - SI-5054
    		allSyms flatMap { makeMember0(_, member) }
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
        if (aSym.hasDefault) {
          // units.filter should return only one element
          (currentRun.units filter (_.source.file == aSym.sourceFile)).toList match {
            case List(unit) =>
              (unit.body find (_.symbol == aSym)) match {
                case Some(ValDef(_,_,_,rhs)) => makeTree(rhs)
                case _ => None
              }
            case _ => None
          }
        }
        else None
      def resultType =
        makeTypeInTemplateContext(sym.tpe, inTpl, sym)
      def isImplicit = aSym.isImplicit
    }

  /** */
  def makeTypeInTemplateContext(aType: Type, inTpl: => TemplateImpl, dclSym: Symbol): TypeEntity = {
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
  def makeType(aType: Type, inTpl: => TemplateImpl): TypeEntity = {
    def templatePackage = closestPackage(inTpl.sym)

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

      private def appendType0(tpe: Type): Unit = tpe match {
        /* Type refs */
        case tp: TypeRef if definitions.isFunctionType(tp) =>
          val args = tp.normalize.typeArgs
          nameBuffer append '('
          appendTypes0(args.init, ", ")
          nameBuffer append ") ⇒ "
          appendType0(args.last)
        case tp: TypeRef if definitions.isScalaRepeatedParamType(tp) =>
          appendType0(tp.args.head)
          nameBuffer append '*'
        case tp: TypeRef if definitions.isByNameParamType(tp) =>
          nameBuffer append "⇒ "
          appendType0(tp.args.head)
        case tp: TypeRef if definitions.isTupleTypeOrSubtype(tp) =>
          val args = tp.normalize.typeArgs
          nameBuffer append '('
          appendTypes0(args, ", ")
          nameBuffer append ')'
        case TypeRef(pre, aSym, targs) =>
          val preSym = pre.widen.typeSymbol
          // There's a work in progress here trying to deal with the
          // places where undesirable prefixes are printed.
          // ...
          // If the prefix is something worthy of printing, see if the prefix type
          // is in the same package as the enclosing template.  If so, print it
          // unqualified and they'll figure it out.
          //
          // val stripPrefixes = List(templatePackage.fullName + ".", "package.", "java.lang.")
          // if (!preSym.printWithoutPrefix) {
          //   nameBuffer append stripPrefixes.foldLeft(pre.prefixString)(_ stripPrefix _)
          // }
          val bSym = normalizeTemplate(aSym)
          if (bSym.isNonClassType)
            nameBuffer append bSym.decodedName
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
          val ignoreParents = Set(AnyClass, ObjectClass)
          val filtParents = parents filterNot (x => ignoreParents(x.typeSymbol)) match {
            case Nil    => parents
            case ps     => ps
          }
          appendTypes0(filtParents, " with ")
          // XXX Still todo: properly printing refinements.
          // Since I didn't know how to go about displaying a multi-line type, I went with
          // printing single method refinements (which should be the most common) and printing
          // the number of members if there are more.
          defs.toList match {
            case Nil      => ()
            case x :: Nil => nameBuffer append (" { " + x.defString + " }")
            case xs       => nameBuffer append (" { ... /* %d definitions in type refinement */ }" format xs.size)
          }
        /* Eval-by-name types */
        case NullaryMethodType(result) =>
          nameBuffer append '⇒'
          appendType0(result)
        /* Polymorphic types */
        case PolyType(tparams, result) => assert(tparams nonEmpty)
//          throw new Error("Polymorphic type '" + tpe + "' cannot be printed as a type")
          def typeParamsToString(tps: List[Symbol]): String = if(tps isEmpty) "" else
            tps.map{tparam =>
              tparam.varianceString + tparam.name + typeParamsToString(tparam.typeParams)
            }.mkString("[", ", ", "]")
          nameBuffer append typeParamsToString(tparams)
          appendType0(result)
        case tpen =>
          nameBuffer append tpen.toString
      }
      appendType0(aType)
      val refEntity = refBuffer
      val name = optimize(nameBuffer.toString)
    }
  }

  def templateShouldDocument(aSym: Symbol): Boolean = {
  	// TODO: document sourceless entities (e.g., Any, etc), based on a new Setting to be added
  	(aSym.isPackageClass || (aSym.sourceFile != null)) && localShouldDocument(aSym) &&
    ( aSym.owner == NoSymbol || templateShouldDocument(aSym.owner) ) && !isEmptyJavaObject(aSym)
  }

  def isEmptyJavaObject(aSym: Symbol): Boolean = {
    def hasMembers = aSym.info.members.exists(s => localShouldDocument(s) && (!s.isConstructor || s.owner == aSym))
    aSym.isModule && aSym.isJavaDefined && !hasMembers
  }

  def localShouldDocument(aSym: Symbol): Boolean = {
    !aSym.isPrivate && (aSym.isProtected || aSym.privateWithin == NoSymbol) && !aSym.isSynthetic
  }
}
