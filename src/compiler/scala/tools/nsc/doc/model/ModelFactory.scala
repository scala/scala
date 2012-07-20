/* NSC -- new Scala compiler -- Copyright 2007-2011 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import diagram._

import scala.collection._
import scala.util.matching.Regex

import symtab.Flags

import io._

import model.{ RootPackage => RootPackageEntity }

/** This trait extracts all required information for documentation from compilation units */
class ModelFactory(val global: Global, val settings: doc.Settings) {
  thisFactory: ModelFactory
               with ModelFactoryImplicitSupport
               with ModelFactoryTypeSupport
               with DiagramFactory
               with CommentFactory
               with TreeFactory
               with MemberLookup =>

  import global._
  import definitions.{ ObjectClass, NothingClass, AnyClass, AnyValClass, AnyRefClass }
  import rootMirror.{ RootPackage, RootClass, EmptyPackage }

  def templatesCount = docTemplatesCache.count(_._2.isDocTemplate) - droppedPackages.size

  private var _modelFinished = false
  def modelFinished: Boolean = _modelFinished
  private var universe: Universe = null

  private def dbg(msg: String) = if (sys.props contains "scala.scaladoc.debug") println(msg)
  protected def closestPackage(sym: Symbol) = {
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

  def makeModel: Option[Universe] = {
    val universe = new Universe { thisUniverse =>
      thisFactory.universe = thisUniverse
      val settings = thisFactory.settings
      val rootPackage = modelCreation.createRootPackage
    }
    _modelFinished = true
    // complete the links between model entities, everthing that couldn't have been done before
    universe.rootPackage.completeModel

    Some(universe) filter (_.rootPackage != null)
  }

  // state:
  var ids = 0
  private val droppedPackages = mutable.Set[PackageImpl]()
  protected val docTemplatesCache = new mutable.LinkedHashMap[Symbol, DocTemplateImpl]
  protected val noDocTemplatesCache = new mutable.LinkedHashMap[Symbol, NoDocTemplateImpl]
  def packageDropped(tpl: DocTemplateImpl) = tpl match {
    case p: PackageImpl => droppedPackages(p)
    case _ => false
  }

  def optimize(str: String): String =
    if (str.length < 16) str.intern else str

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */

  abstract class EntityImpl(val sym: Symbol, val inTpl: TemplateImpl) extends Entity {
    val id = { ids += 1; ids }
    val name = optimize(sym.nameString)
    val universe = thisFactory.universe

    // Debugging:
    // assert(id != 36, sym + "  " + sym.getClass)
    //println("Creating entity #" + id + " [" + kind + " " + qualifiedName + "] for sym " + sym.kindString + " " + sym.ownerChain.reverse.map(_.name).mkString("."))

    def inTemplate: TemplateImpl = inTpl
    def toRoot: List[EntityImpl] = this :: inTpl.toRoot
    def qualifiedName = name
    def annotations = sym.annotations.map(makeAnnotation)
    def inPackageObject: Boolean = sym.owner.isModuleClass && sym.owner.sourceModule.isPackageObject
    def isType = sym.name.isTypeName
    def isTerm = sym.name.isTermName
  }

  trait TemplateImpl extends EntityImpl with TemplateEntity {
    override def qualifiedName: String =
      if (inTemplate == null || inTemplate.isRootPackage) name else optimize(inTemplate.qualifiedName + "." + name)
    def isPackage = sym.isPackage
    def isTrait = sym.isTrait
    def isClass = sym.isClass && !sym.isTrait
    def isObject = sym.isModule && !sym.isPackage
    def isCaseClass = sym.isCaseClass
    def isRootPackage = false
    def isNoDocMemberTemplate = false
    def selfType = if (sym.thisSym eq sym) None else Some(makeType(sym.thisSym.typeOfThis, this))
  }

  abstract class MemberImpl(sym: Symbol, inTpl: DocTemplateImpl) extends EntityImpl(sym, inTpl) with MemberEntity {
    lazy val comment = {
      // If the current tpl is a DocTemplate, we consider itself as the root for resolving link targets (instead of the
      // package the class is in) -- so people can refer to methods directly [[foo]], instead of using [[MyClass.foo]]
      // in the doc comment of MyClass
      val thisTpl = this match {
        case d: DocTemplateImpl => Some(d)
        case _ => None
      }
      if (inTpl != null) thisFactory.comment(sym, thisTpl, inTpl) else None
    }
    def group = if (comment.isDefined) comment.get.group.getOrElse("No Group") else "No Group"
    override def inTemplate = inTpl
    override def toRoot: List[MemberImpl] = this :: inTpl.toRoot
    def inDefinitionTemplates = this match {
        case mb: NonTemplateMemberEntity if (mb.useCaseOf.isDefined) =>
          mb.useCaseOf.get.inDefinitionTemplates
        case _ =>
          if (inTpl == null)
            List(makeRootPackage)
          else
            makeTemplate(sym.owner)::(sym.allOverriddenSymbols map { inhSym => makeTemplate(inhSym.owner) })
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
      /* Resetting the DEFERRED flag is a little trick here for refined types: (example from scala.collections)
       * {{{
       *     implicit def traversable2ops[T](t: collection.GenTraversableOnce[T]) = new TraversableOps[T] {
       *       def isParallel = ...
       * }}}
       * the type the method returns is TraversableOps, which has all-abstract symbols. But in reality, it couldn't have
       * any abstract terms, otherwise it would fail compilation. So we reset the DEFERRED flag. */
      if (!sym.isTrait && (sym hasFlag Flags.DEFERRED) && (!isImplicitlyInherited)) fgs += Paragraph(Text("abstract"))
      if (!sym.isModule && (sym hasFlag Flags.FINAL)) fgs += Paragraph(Text("final"))
      fgs.toList
    }
    def deprecation =
      if (sym.isDeprecated)
        Some((sym.deprecationMessage, sym.deprecationVersion) match {
          case (Some(msg), Some(ver)) => parseWiki("''(Since version " + ver + ")'' " + msg, NoPosition, Some(inTpl))
          case (Some(msg), None) => parseWiki(msg, NoPosition, Some(inTpl))
          case (None, Some(ver)) =>  parseWiki("''(Since version " + ver + ")''", NoPosition, Some(inTpl))
          case (None, None) => Body(Nil)
        })
      else
        comment flatMap { _.deprecated }
    def migration =
      if(sym.hasMigrationAnnotation)
        Some((sym.migrationMessage, sym.migrationVersion) match {
          case (Some(msg), Some(ver)) => parseWiki("''(Changed in version " + ver + ")'' " + msg, NoPosition, Some(inTpl))
          case (Some(msg), None) => parseWiki(msg, NoPosition, Some(inTpl))
          case (None, Some(ver)) =>  parseWiki("''(Changed in version " + ver + ")''", NoPosition, Some(inTpl))
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
      val tpe = if (!isImplicitlyInherited) sym.tpe else byConversion.get.toType memberInfo sym
      makeTypeInTemplateContext(resultTpe(tpe), inTemplate, sym)
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
      // for the explanation of conversion == null see comment on flags
      ((!sym.isTrait && ((sym hasFlag Flags.ABSTRACT) || (sym hasFlag Flags.DEFERRED)) && (!isImplicitlyInherited)) ||
      sym.isAbstractClass || sym.isAbstractType) && !sym.isSynthetic
    def isTemplate = false
    lazy val signature = {

      def defParams(mbr: Any): String = mbr match {
        case d: MemberEntity with Def =>
          val paramLists: List[String] =
            if (d.valueParams.isEmpty) Nil
            else d.valueParams map (ps => ps map (_.resultType.name) mkString ("(",",",")"))
          paramLists.mkString
        case _ => ""
      }

      def tParams(mbr: Any): String = mbr match {
        case hk: HigherKinded if !hk.typeParams.isEmpty =>
          def boundsToString(hi: Option[TypeEntity], lo: Option[TypeEntity]): String = {
            def bound0(bnd: Option[TypeEntity], pre: String): String = bnd match {
              case None => ""
              case Some(tpe) => pre ++ tpe.toString
            }
            bound0(hi, "<:") ++ bound0(lo, ">:")
          }
          "[" + hk.typeParams.map(tp => tp.variance + tp.name + tParams(tp) + boundsToString(tp.hi, tp.lo)).mkString(", ") + "]"
        case _ => ""
      }

      (name + tParams(this) + defParams(this) +":"+ resultType.name).replaceAll("\\s","") // no spaces allowed, they break links
    }
    // these only apply for NonTemplateMemberEntities
    def useCaseOf: Option[MemberEntity] = None
    def byConversion: Option[ImplicitConversionImpl] = None
    def isImplicitlyInherited = false
    def isShadowedImplicit    = false
    def isAmbiguousImplicit   = false
    def isShadowedOrAmbiguousImplicit = false
  }

  /** A template that is not documented at all. The class is instantiated during lookups, to indicate that the class
   *  exists, but should not be documented (either it's not included in the source or it's not visible)
   */
  class NoDocTemplateImpl(sym: Symbol, inTpl: TemplateImpl) extends EntityImpl(sym, inTpl) with TemplateImpl with HigherKindedImpl with NoDocTemplate {
    assert(modelFinished)
    assert(!(noDocTemplatesCache isDefinedAt sym))
    noDocTemplatesCache += (sym -> this)
    def isDocTemplate = false
  }

  /** An inherited template that was not documented in its original owner - example:
   *  in classpath:  trait T { class C } -- T (and implicitly C) are not documented
   *  in the source: trait U extends T -- C appears in U as a MemberTemplateImpl -- that is, U has a member for it
   *  but C doesn't get its own page
   */
  abstract class MemberTemplateImpl(sym: Symbol, inTpl: DocTemplateImpl) extends MemberImpl(sym, inTpl) with TemplateImpl with HigherKindedImpl with MemberTemplateEntity {
    // no templates cache for this class, each owner gets its own instance
    override def isTemplate = true
    def isDocTemplate = false
    override def isNoDocMemberTemplate = true
    lazy val definitionName = optimize(inDefinitionTemplates.head.qualifiedName + "." + name)
    def valueParams: List[List[ValueParam]] = Nil /** TODO, these are now only computed for DocTemplates */

    // Seems unused
    // def parentTemplates =
    //   if (sym.isPackage || sym == AnyClass)
    //     List()
    //   else
    //     sym.tpe.parents.flatMap { tpe: Type =>
    //       val tSym = tpe.typeSymbol
    //       if (tSym != NoSymbol)
    //         List(makeTemplate(tSym))
    //       else
    //         List()
    //     } filter (_.isInstanceOf[DocTemplateEntity])

    def parentTypes =
      if (sym.isPackage || sym == AnyClass) List() else {
        val tps = (this match {
          case a: AliasType => sym.tpe.dealias.parents
          case a: AbstractType => sym.info.bounds match {
            case TypeBounds(lo, hi) => List(hi)
            case _ => Nil
          }
          case _ => sym.tpe.parents
        }) map { _.asSeenFrom(sym.thisType, sym) }
        makeParentTypes(RefinedType(tps, EmptyScope), Some(this), inTpl)
      }
  }

   /** The instantiation of `TemplateImpl` triggers the creation of the following entities:
    *  All ancestors of the template and all non-package members.
    */
  abstract class DocTemplateImpl(sym: Symbol, inTpl: DocTemplateImpl) extends MemberTemplateImpl(sym, inTpl) with DocTemplateEntity {
    assert(!modelFinished)
    assert(!(docTemplatesCache isDefinedAt sym), sym)
    docTemplatesCache += (sym -> this)

    if (settings.verbose.value)
      inform("Creating doc template for " + sym)

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

    protected def linearizationFromSymbol(symbol: Symbol): List[(TemplateEntity, TypeEntity)] = {
      symbol.ancestors map { ancestor =>
        val typeEntity = makeType(symbol.info.baseType(ancestor), this)
        val tmplEntity = makeTemplate(ancestor) match {
          case tmpl: DocTemplateImpl  => tmpl registerSubClass this ; tmpl
          case tmpl                   => tmpl
        }
        (tmplEntity, typeEntity)
      }
    }

    lazy val linearization = linearizationFromSymbol(sym)
    def linearizationTemplates = linearization map { _._1 }
    def linearizationTypes = linearization map { _._2 }

    /* Subclass cache */
    private lazy val subClassesCache = (
      if (sym == AnyRefClass) null
      else mutable.ListBuffer[DocTemplateEntity]()
    )
    def registerSubClass(sc: DocTemplateEntity): Unit = {
      if (subClassesCache != null)
        subClassesCache += sc
    }
    def allSubClasses = if (subClassesCache == null) Nil else subClassesCache.toList
    def directSubClasses = allSubClasses.filter(_.parentTypes.map(_._1).contains(this))

    /* Implcitly convertible class cache */
    private var implicitlyConvertibleClassesCache: mutable.ListBuffer[(DocTemplateImpl, ImplicitConversionImpl)] = null
    def registerImplicitlyConvertibleClass(dtpl: DocTemplateImpl, conv: ImplicitConversionImpl): Unit = {
      if (implicitlyConvertibleClassesCache == null)
        implicitlyConvertibleClassesCache = mutable.ListBuffer[(DocTemplateImpl, ImplicitConversionImpl)]()
      implicitlyConvertibleClassesCache += ((dtpl, conv))
    }

    def incomingImplicitlyConvertedClasses: List[(DocTemplateImpl, ImplicitConversionImpl)] =
      if (implicitlyConvertibleClassesCache == null)
        List()
      else
        implicitlyConvertibleClassesCache.toList

    // the implicit conversions are generated eagerly, but the members generated by implicit conversions are added
    // lazily, on completeModel
    val conversions: List[ImplicitConversionImpl] =
      if (settings.docImplicits.value) makeImplicitConversions(sym, this) else Nil

    // members as given by the compiler
    lazy val memberSyms      = sym.info.members.filter(s => membersShouldDocument(s, this))

    // the inherited templates (classes, traits or objects)
    var memberSymsLazy  = memberSyms.filter(t => templateShouldDocument(t, this) && !inOriginalOwner(t, this))
    // the direct members (methods, values, vars, types and directly contained templates)
    var memberSymsEager = memberSyms.filter(!memberSymsLazy.contains(_))
    // the members generated by the symbols in memberSymsEager
    val ownMembers      = (memberSymsEager.flatMap(makeMember(_, None, this)))

    // all the members that are documentented PLUS the members inherited by implicit conversions
    var members: List[MemberImpl] = ownMembers

    def templates       = members collect { case c: TemplateEntity with MemberEntity => c }
    def methods         = members collect { case d: Def => d }
    def values          = members collect { case v: Val => v }
    def abstractTypes   = members collect { case t: AbstractType => t }
    def aliasTypes      = members collect { case t: AliasType => t }

    /**
     * This is the final point in the core model creation: no DocTemplates are created after the model has finished, but
     * inherited templates and implicit members are added to the members at this point.
     */
    def completeModel: Unit = {
      // DFS completion
      // since alias types and abstract types have no own members, there's no reason for them to call completeModel
      if (!sym.isAliasType && !sym.isAbstractType)
        for (member <- members)
          member match {
            case d: DocTemplateImpl => d.completeModel
            case _ =>
          }

      members :::= memberSymsLazy.map(modelCreation.createLazyTemplateMember(_, this))

      // compute linearization to register subclasses
      linearization
      outgoingImplicitlyConvertedClasses

      // the members generated by the symbols in memberSymsEager PLUS the members from the usecases
      val allMembers = ownMembers ::: ownMembers.flatMap(_.useCaseOf.map(_.asInstanceOf[MemberImpl])).distinct
      implicitsShadowing = makeShadowingTable(allMembers, conversions, this)
      // finally, add the members generated by implicit conversions
      members :::= conversions.flatMap(_.memberImpls)
    }

    var implicitsShadowing = Map[MemberEntity, ImplicitMemberShadowing]()

    lazy val outgoingImplicitlyConvertedClasses: List[(TemplateEntity, TypeEntity, ImplicitConversionImpl)] =
      conversions flatMap (conv =>
        if (!implicitExcluded(conv.conversionQualifiedName))
          conv.targetTypeComponents map {
            case pair@(template, tpe) =>
              template match {
                case d: DocTemplateImpl if (d != this) => d.registerImplicitlyConvertibleClass(this, conv)
                case _ => // nothing
              }
              (pair._1, pair._2, conv)
          }
        else List()
      )

    override def isTemplate = true
    override def isDocTemplate = true
    private[this] lazy val companionSymbol =
      if (sym.isAliasType || sym.isAbstractType) {
        inTpl.sym.info.member(sym.name.toTermName) match {
          case NoSymbol => NoSymbol
          case s =>
            s.info match {
              case ot: OverloadedType =>
                NoSymbol
              case _ =>
                // that's to navigate from val Foo: FooExtractor to FooExtractor :)
                s.info.resultType.typeSymbol
            }
        }
      }
      else
        sym.companionSymbol

    def companion =
      companionSymbol match {
        case NoSymbol => None
        case comSym if !isEmptyJavaObject(comSym) && (comSym.isClass || comSym.isModule) =>
          makeTemplate(comSym) match {
            case d: DocTemplateImpl => Some(d)
            case _ => None
          }
        case _ => None
      }

    def constructors: List[MemberImpl with Constructor] = if (isClass) members collect { case d: Constructor => d } else Nil
    def primaryConstructor: Option[MemberImpl with Constructor] = if (isClass) constructors find { _.isPrimary } else None
    override def valueParams =
      // we don't want params on a class (non case class) signature
      if (isCaseClass) primaryConstructor match {
        case Some(const) => const.sym.paramss map (_ map (makeValueParam(_, this)))
        case None => List()
      }
      else List.empty

    // These are generated on-demand, make sure you don't call them more than once
    def inheritanceDiagram = makeInheritanceDiagram(this)
    def contentDiagram = makeContentDiagram(this)

    def groupSearch[T](extractor: Comment => T, default: T): T = {
      // query this template
      if (comment.isDefined) {
        val entity = extractor(comment.get)
        if (entity != default) return entity
      }
      // query linearization
      for (tpl <- linearizationTemplates.collect{ case dtpl: DocTemplateImpl if dtpl!=this => dtpl}) {
        val entity = tpl.groupSearch(extractor, default)
        if (entity != default) return entity
      }
      default
    }

    def groupDescription(group: String): Option[Body] = groupSearch(_.groupDesc.get(group), None)
    def groupPriority(group: String): Int = groupSearch(_.groupPrio.get(group) match { case Some(prio) => prio; case _ => 0 }, 0)
    def groupName(group: String): String = groupSearch(_.groupNames.get(group) match { case Some(name) => name; case _ => group }, group)
  }

  abstract class PackageImpl(sym: Symbol, inTpl: PackageImpl) extends DocTemplateImpl(sym, inTpl) with Package {
    override def inTemplate = inTpl
    override def toRoot: List[PackageImpl] = this :: inTpl.toRoot
    override lazy val linearization = {
      val symbol = sym.info.members.find {
        s => s.isPackageObject
      } getOrElse sym
      linearizationFromSymbol(symbol)
    }
    def packages = members collect { case p: PackageImpl if !(droppedPackages contains p) => p }
  }

  abstract class RootPackageImpl(sym: Symbol) extends PackageImpl(sym, null) with RootPackageEntity

  abstract class NonTemplateMemberImpl(sym: Symbol, conversion: Option[ImplicitConversionImpl],
                                       override val useCaseOf: Option[MemberEntity], inTpl: DocTemplateImpl)
           extends MemberImpl(sym, inTpl) with NonTemplateMemberEntity {
    override lazy val comment = {
      val inRealTpl =
        /* Variable precendence order for implicitly added members: Take the variable defifinitions from ...
         * 1. the target of the implicit conversion
         * 2. the definition template (owner)
         * 3. the current template
         */
        if (conversion.isDefined) findTemplateMaybe(conversion.get.toType.typeSymbol) match {
          case Some(d) if d != makeRootPackage => d //in case of NoSymbol, it will give us the root package
          case _ => findTemplateMaybe(sym.owner) match {
            case Some(d) if d != makeRootPackage => d //in case of NoSymbol, it will give us the root package
            case _ => inTpl
          }
        } else inTpl
      if (inRealTpl != null) thisFactory.comment(sym, None, inRealTpl) else None
    }

    override def qualifiedName = optimize(inTemplate.qualifiedName + "#" + name)
    lazy val definitionName = {
      // this contrived name is here just to satisfy some older tests -- if you decide to remove it, be my guest, and
      // also remove property("package object") from test/scaladoc/scalacheck/HtmlFactoryTest.scala so you don't break
      // the test suite...
      val packageObject = if (inPackageObject) ".package" else ""
      if (!conversion.isDefined) optimize(inDefinitionTemplates.head.qualifiedName + packageObject + "#" + name)
      else                       optimize(conversion.get.conversionQualifiedName + packageObject + "#" + name)
    }
    def isBridge = sym.isBridge
    def isUseCase = useCaseOf.isDefined
    override def byConversion: Option[ImplicitConversionImpl] = conversion
    override def isImplicitlyInherited = { assert(modelFinished); conversion.isDefined }
    override def isShadowedImplicit    = isImplicitlyInherited && inTpl.implicitsShadowing.get(this).map(_.isShadowed).getOrElse(false)
    override def isAmbiguousImplicit   = isImplicitlyInherited && inTpl.implicitsShadowing.get(this).map(_.isAmbiguous).getOrElse(false)
    override def isShadowedOrAmbiguousImplicit = isShadowedImplicit || isAmbiguousImplicit
  }

  abstract class NonTemplateParamMemberImpl(sym: Symbol, conversion: Option[ImplicitConversionImpl],
                                            useCaseOf: Option[MemberEntity], inTpl: DocTemplateImpl)
           extends NonTemplateMemberImpl(sym, conversion, useCaseOf, inTpl) {
    def valueParams = {
      val info = if (!isImplicitlyInherited) sym.info else conversion.get.toType memberInfo sym
      info.paramss map { ps => (ps.zipWithIndex) map { case (p, i) =>
        if (p.nameString contains "$") makeValueParam(p, inTpl, optimize("arg" + i)) else makeValueParam(p, inTpl)
      }}
    }
  }

  abstract class ParameterImpl(val sym: Symbol, val inTpl: TemplateImpl) extends ParameterEntity {
    val name = optimize(sym.nameString)
  }

  private trait AliasImpl {
    def sym: Symbol
    def inTpl: TemplateImpl
    def alias = makeTypeInTemplateContext(sym.tpe.dealias, inTpl, sym)
  }

  private trait TypeBoundsImpl {
    def sym: Symbol
    def inTpl: TemplateImpl
    def lo = sym.info.bounds match {
      case TypeBounds(lo, hi) if lo.typeSymbol != NothingClass =>
        Some(makeTypeInTemplateContext(appliedType(lo, sym.info.typeParams map {_.tpe}), inTpl, sym))
      case _ => None
    }
    def hi = sym.info.bounds match {
      case TypeBounds(lo, hi) if hi.typeSymbol != AnyClass =>
        Some(makeTypeInTemplateContext(appliedType(hi, sym.info.typeParams map {_.tpe}), inTpl, sym))
      case _ => None
    }
  }

  trait HigherKindedImpl extends HigherKinded {
    def sym: Symbol
    def inTpl: TemplateImpl
    def typeParams =
      sym.typeParams map (makeTypeParam(_, inTpl))
  }
  /* ============== MAKER METHODS ============== */

  /** This method makes it easier to work with the different kinds of symbols created by scalac by stripping down the
   * package object abstraction and placing members directly in the package.
   *
   * Here's the explanation of what we do. The code:
   *
   * package foo {
   *   object `package` {
   *     class Bar
   *   }
   * }
   *
   * will yield this Symbol structure:
   *                                       +---------+ (2)
   *                                       |         |
   * +---------------+         +---------- v ------- | ---+                              +--------+ (2)
   * | package foo#1 <---(1)---- module class foo#2  |    |                              |        |
   * +---------------+         | +------------------ | -+ |         +------------------- v ---+   |
   *                           | | package object foo#3 <-----(1)---- module class package#4  |   |
   *                           | +----------------------+ |         | +---------------------+ |   |
   *                           +--------------------------+         | | class package$Bar#5 | |   |
   *                                                                | +----------------- | -+ |   |
   *                                                                +------------------- | ---+   |
   *                                                                                     |        |
   *                                                                                     +--------+
   * (1) sourceModule
   * (2) you get out of owners with .owner
   *
   * and normalizeTemplate(Bar.owner) will get us the package, instead of the module class of the package object.
   */
  def normalizeTemplate(aSym: Symbol): Symbol = aSym match {
    case null | rootMirror.EmptyPackage | NoSymbol =>
      normalizeTemplate(RootPackage)
    case ObjectClass =>
      normalizeTemplate(AnyRefClass)
    case _ if aSym.isPackageObject =>
      normalizeTemplate(aSym.owner)
    case _ if aSym.isModuleClass =>
      normalizeTemplate(aSym.sourceModule)
    case _ =>
      aSym
  }

  /**
   * These are all model construction methods. Please do not use them directly, they are calling each other recursively
   * starting from makeModel. On the other hand, makeTemplate, makeAnnotation, makeMember, makeType should only be used
   * after the model was created (modelFinished=true) otherwise assertions will start failing.
   */
  object modelCreation {

    def createRootPackage: PackageImpl = docTemplatesCache.get(RootPackage) match {
      case Some(root: PackageImpl) => root
      case _ => modelCreation.createTemplate(RootPackage, null) match {
        case Some(root: PackageImpl) => root
        case _ => sys.error("Scaladoc: Unable to create root package!")
      }
    }

    /**
     *  Create a template, either a package, class, trait or object
     */
    def createTemplate(aSym: Symbol, inTpl: DocTemplateImpl): Option[MemberImpl] = {
      // don't call this after the model finished!
      assert(!modelFinished)

      def createRootPackageComment: Option[Comment] =
        if(settings.docRootContent.isDefault) None
        else {
          import Streamable._
          Path(settings.docRootContent.value) match {
            case f : File => {
              val rootComment = closing(f.inputStream)(is => parse(slurp(is), "", NoPosition, Option(inTpl)))
              Some(rootComment)
            }
            case _ => None
          }
        }

      def createDocTemplate(bSym: Symbol, inTpl: DocTemplateImpl): DocTemplateImpl = {
        assert(!modelFinished) // only created BEFORE the model is finished
        if (bSym.isAliasType && bSym != AnyRefClass)
          new DocTemplateImpl(bSym, inTpl) with AliasImpl with AliasType { override def isAliasType = true }
        else if (bSym.isAbstractType)
          new DocTemplateImpl(bSym, inTpl) with TypeBoundsImpl with AbstractType { override def isAbstractType = true }
        else if (bSym.isModule)
          new DocTemplateImpl(bSym, inTpl) with Object {}
        else if (bSym.isTrait)
          new DocTemplateImpl(bSym, inTpl) with Trait {}
        else if (bSym.isClass || bSym == AnyRefClass)
          new DocTemplateImpl(bSym, inTpl) with Class {}
        else
          sys.error("'" + bSym + "' isn't a class, trait or object thus cannot be built as a documentable template.")
      }

      val bSym = normalizeTemplate(aSym)
      if (docTemplatesCache isDefinedAt bSym)
        return Some(docTemplatesCache(bSym))

      /* Three cases of templates:
       * (1) root package -- special cased for bootstrapping
       * (2) package
       * (3) class/object/trait
       */
      if (bSym == RootPackage) // (1)
        Some(new RootPackageImpl(bSym) {
          override lazy val comment = createRootPackageComment
          override val name = "root"
          override def inTemplate = this
          override def toRoot = this :: Nil
          override def qualifiedName = "_root_"
          override def inheritedFrom = Nil
          override def isRootPackage = true
          override lazy val memberSyms =
            (bSym.info.members ++ EmptyPackage.info.members) filter { s =>
              s != EmptyPackage && s != RootPackage
            }
        })
      else if (bSym.isPackage) // (2)
        if (settings.skipPackage(makeQualifiedName(bSym)))
          None
        else
          inTpl match {
            case inPkg: PackageImpl =>
              val pack = new PackageImpl(bSym, inPkg) {}
              // Used to check package pruning works:
              //println(pack.qualifiedName)
              if (pack.templates.filter(_.isDocTemplate).isEmpty && pack.memberSymsLazy.isEmpty) {
                droppedPackages += pack
                None
              } else
                Some(pack)
            case _ =>
              sys.error("'" + bSym + "' must be in a package")
          }
      else {
        // no class inheritance at this point
        assert(inOriginalOwner(bSym, inTpl) || bSym.isAbstractType || bSym.isAliasType, bSym + " in " + inTpl)
        Some(createDocTemplate(bSym, inTpl))
      }
    }

    /**
     *  After the model is completed, no more DocTemplateEntities are created.
     *  Therefore any symbol that still appears is:
     *   - MemberTemplateEntity (created here)
     *   - NoDocTemplateEntity (created in makeTemplate)
     */
    def createLazyTemplateMember(aSym: Symbol, inTpl: DocTemplateImpl): MemberImpl = {

      // Code is duplicate because the anonymous classes are created statically
      def createNoDocMemberTemplate(bSym: Symbol, inTpl: DocTemplateImpl): MemberTemplateImpl = {
        assert(modelFinished) // only created AFTER the model is finished
        if (bSym.isModule || (bSym.isAliasType && bSym.tpe.typeSymbol.isModule))
          new MemberTemplateImpl(bSym, inTpl) with Object {}
        else if (bSym.isTrait || (bSym.isAliasType && bSym.tpe.typeSymbol.isTrait))
          new MemberTemplateImpl(bSym, inTpl) with Trait {}
        else if (bSym.isClass || (bSym.isAliasType && bSym.tpe.typeSymbol.isClass))
          new MemberTemplateImpl(bSym, inTpl) with Class {}
        else
          sys.error("'" + bSym + "' isn't a class, trait or object thus cannot be built as a member template.")
      }

      assert(modelFinished)
      val bSym = normalizeTemplate(aSym)

      if (docTemplatesCache isDefinedAt bSym)
        docTemplatesCache(bSym)
      else
        docTemplatesCache.get(bSym.owner) match {
          case Some(inTpl) =>
            val mbrs = inTpl.members.collect({ case mbr: MemberImpl if mbr.sym == bSym => mbr })
            assert(mbrs.length == 1)
            mbrs.head
          case _ =>
            // move the class completely to the new location
            createNoDocMemberTemplate(bSym, inTpl)
        }
    }
  }

  /** Get the root package */
  def makeRootPackage: PackageImpl = docTemplatesCache(RootPackage).asInstanceOf[PackageImpl]

  // TODO: Should be able to override the type
  def makeMember(aSym: Symbol, conversion: Option[ImplicitConversionImpl], inTpl: DocTemplateImpl): List[MemberImpl] = {

    def makeMember0(bSym: Symbol, useCaseOf: Option[MemberImpl]): Option[MemberImpl] = {
      if (bSym.isGetter && bSym.isLazy)
          Some(new NonTemplateMemberImpl(bSym, conversion, useCaseOf, inTpl) with Val {
            override lazy val comment = // The analyser does not duplicate the lazy val's DocDef when it introduces its accessor.
              thisFactory.comment(bSym.accessed, None, inTpl.asInstanceOf[DocTemplateImpl]) // This hack should be removed after analyser is fixed.
            override def isLazyVal = true
          })
      else if (bSym.isGetter && bSym.accessed.isMutable)
        Some(new NonTemplateMemberImpl(bSym, conversion, useCaseOf, inTpl) with Val {
          override def isVar = true
        })
      else if (bSym.isMethod && !bSym.hasAccessorFlag && !bSym.isConstructor && !bSym.isModule) {
        val cSym = { // This unsightly hack closes issue #4086.
          if (bSym == definitions.Object_synchronized) {
            val cSymInfo = (bSym.info: @unchecked) match {
              case PolyType(ts, MethodType(List(bp), mt)) =>
                val cp = bp.cloneSymbol.setPos(bp.pos).setInfo(definitions.byNameType(bp.info))
                PolyType(ts, MethodType(List(cp), mt))
            }
            bSym.cloneSymbol.setPos(bSym.pos).setInfo(cSymInfo)
          }
          else bSym
        }
        Some(new NonTemplateParamMemberImpl(cSym, conversion, useCaseOf, inTpl) with HigherKindedImpl with Def {
          override def isDef = true
        })
      }
      else if (bSym.isConstructor)
        if (conversion.isDefined)
          None // don't list constructors inherted by implicit conversion
        else
          Some(new NonTemplateParamMemberImpl(bSym, conversion, useCaseOf, inTpl) with Constructor {
            override def isConstructor = true
            def isPrimary = sym.isPrimaryConstructor
          })
      else if (bSym.isGetter) // Scala field accessor or Java field
        Some(new NonTemplateMemberImpl(bSym, conversion, useCaseOf, inTpl) with Val {
          override def isVal = true
        })
      else if (bSym.isAbstractType && !typeShouldDocument(bSym, inTpl))
        Some(new MemberTemplateImpl(bSym, inTpl) with TypeBoundsImpl with AbstractType {
          override def isAbstractType = true
        })
      else if (bSym.isAliasType && !typeShouldDocument(bSym, inTpl))
        Some(new MemberTemplateImpl(bSym, inTpl) with AliasImpl with AliasType {
          override def isAliasType = true
        })
      else if (!modelFinished && (bSym.isPackage || bSym.isAliasType || bSym.isAbstractType || templateShouldDocument(bSym, inTpl)))
        modelCreation.createTemplate(bSym, inTpl)
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

  def findMember(aSym: Symbol, inTpl: DocTemplateImpl): Option[MemberImpl] = {
    val tplSym = normalizeTemplate(aSym.owner)
    inTpl.members.find(_.sym == aSym)
  }

  @deprecated("2.10", "Use findLinkTarget instead!")
  def findTemplate(query: String): Option[DocTemplateImpl] = {
    assert(modelFinished)
    docTemplatesCache.values find { (tpl: DocTemplateImpl) => tpl.qualifiedName == query && !packageDropped(tpl) && !tpl.isObject }
  }

  def findTemplateMaybe(aSym: Symbol): Option[DocTemplateImpl] = {
    assert(modelFinished)
    docTemplatesCache.get(normalizeTemplate(aSym)).filterNot(packageDropped(_))
  }

  def makeTemplate(aSym: Symbol): TemplateImpl = makeTemplate(aSym, None)

  def makeTemplate(aSym: Symbol, inTpl: Option[TemplateImpl]): TemplateImpl = {
    assert(modelFinished)

    def makeNoDocTemplate(aSym: Symbol, inTpl: TemplateImpl): NoDocTemplateImpl = {
      val bSym = normalizeTemplate(aSym)
      noDocTemplatesCache.get(bSym) match {
        case Some(noDocTpl) => noDocTpl
        case None => new NoDocTemplateImpl(bSym, inTpl)
      }
    }

    findTemplateMaybe(aSym) match {
      case Some(dtpl) =>
        dtpl
      case None =>
        val bSym = normalizeTemplate(aSym)
        makeNoDocTemplate(bSym, if (inTpl.isDefined) inTpl.get else makeTemplate(bSym.owner))
    }
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
          case aClass: DocTemplateEntity with Class =>
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
  def makeTypeParam(aSym: Symbol, inTpl: TemplateImpl): TypeParam =
    new ParameterImpl(aSym, inTpl) with TypeBoundsImpl with HigherKindedImpl with TypeParam {
      def variance: String = {
        if (sym hasFlag Flags.COVARIANT) "+"
        else if (sym hasFlag Flags.CONTRAVARIANT) "-"
        else ""
      }
    }

  /** */
  def makeValueParam(aSym: Symbol, inTpl: DocTemplateImpl): ValueParam = {
    makeValueParam(aSym, inTpl, aSym.nameString)
  }


  /** */
  def makeValueParam(aSym: Symbol, inTpl: DocTemplateImpl, newName: String): ValueParam =
    new ParameterImpl(aSym, inTpl) with ValueParam {
      override val name = newName
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
        makeTypeInTemplateContext(aSym.tpe, inTpl, aSym)
      def isImplicit = aSym.isImplicit
    }

  /** */
  def makeTypeInTemplateContext(aType: Type, inTpl: TemplateImpl, dclSym: Symbol): TypeEntity = {
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

  /** Get the types of the parents of the current class, ignoring the refinements */
  def makeParentTypes(aType: Type, tpl: Option[MemberTemplateImpl], inTpl: TemplateImpl): List[(TemplateEntity, TypeEntity)] = aType match {
    case RefinedType(parents, defs) =>
      val ignoreParents = Set[Symbol](AnyClass, AnyRefClass, ObjectClass)
      val filtParents =
        // we don't want to expose too many links to AnyRef, that will just be redundant information
        if (tpl.isDefined && { val sym = tpl.get.sym; (!sym.isModule && parents.length < 2) || (sym == AnyValClass) || (sym == AnyRefClass) || (sym == AnyClass) })
          parents
        else
          parents.filterNot((p: Type) => ignoreParents(p.typeSymbol))

      /** Returns:
       *   - a DocTemplate if the type's symbol is documented
       *   - a NoDocTemplateMember if the type's symbol is not documented in its parent but in another template
       *   - a NoDocTemplate if the type's symbol is not documented at all */
      def makeTemplateOrMemberTemplate(parent: Type): TemplateImpl = {
        def noDocTemplate = makeTemplate(parent.typeSymbol)
        findTemplateMaybe(parent.typeSymbol) match {
          case Some(tpl) => tpl
          case None => parent match {
            case TypeRef(pre, sym, args) =>
              findTemplateMaybe(pre.typeSymbol) match {
                case Some(tpl) => findMember(parent.typeSymbol, tpl).collect({case t: TemplateImpl => t}).getOrElse(noDocTemplate)
                case None => noDocTemplate
              }
            case _ => noDocTemplate
          }
        }
      }

      filtParents.map(parent => {
        val templateEntity = makeTemplateOrMemberTemplate(parent)
        val typeEntity = makeType(parent, inTpl)
        (templateEntity, typeEntity)
      })
    case _ =>
      List((makeTemplate(aType.typeSymbol), makeType(aType, inTpl)))
  }

  def makeQualifiedName(sym: Symbol, relativeTo: Option[Symbol] = None): String = {
    val stop = if (relativeTo.isDefined) relativeTo.get.ownerChain.toSet else Set[Symbol]()
    var sym1 = sym
    var path = new StringBuilder()
    // var path = List[Symbol]()

    while ((sym1 != NoSymbol) && (path.isEmpty || !stop(sym1))) {
      val sym1Norm = normalizeTemplate(sym1)
      if (!sym1.sourceModule.isPackageObject && sym1Norm != RootPackage) {
        if (path.length != 0)
          path.insert(0, ".")
        path.insert(0, sym1Norm.nameString)
        // path::= sym1Norm
      }
      sym1 = sym1.owner
    }

    optimize(path.toString)
    //path.mkString(".")
  }

  def inOriginalOwner(aSym: Symbol, inTpl: TemplateImpl): Boolean =
    normalizeTemplate(aSym.owner) == normalizeTemplate(inTpl.sym)

  def templateShouldDocument(aSym: Symbol, inTpl: TemplateImpl): Boolean =
    (aSym.isTrait || aSym.isClass || aSym.isModule) &&
    localShouldDocument(aSym) &&
    !isEmptyJavaObject(aSym) &&
    // either it's inside the original owner or we can document it later:
    (!inOriginalOwner(aSym, inTpl) || (aSym.isPackageClass || (aSym.sourceFile != null)))

  def membersShouldDocument(sym: Symbol, inTpl: TemplateImpl) = {
    // pruning modules that shouldn't be documented
    // Why Symbol.isInitialized? Well, because we need to avoid exploring all the space available to scaladoc
    // from the classpath -- scaladoc is a hog, it will explore everything starting from the root package unless we
    // somehow prune the tree. And isInitialized is a good heuristic for prunning -- if the package was not explored
    // during typer and refchecks, it's not necessary for the current application and there's no need to explore it.
    (!sym.isModule || sym.moduleClass.isInitialized) &&
    // documenting only public and protected members
    localShouldDocument(sym) &&
    // Only this class's constructors are part of its members, inherited constructors are not.
    (!sym.isConstructor || sym.owner == inTpl.sym) &&
    // If the @bridge annotation overrides a normal member, show it
    !isPureBridge(sym)
  }

  def isEmptyJavaObject(aSym: Symbol): Boolean =
    aSym.isModule && aSym.isJavaDefined &&
    aSym.info.members.exists(s => localShouldDocument(s) && (!s.isConstructor || s.owner == aSym))

  def localShouldDocument(aSym: Symbol): Boolean =
    !aSym.isPrivate && (aSym.isProtected || aSym.privateWithin == NoSymbol) && !aSym.isSynthetic

  /** Filter '@bridge' methods only if *they don't override non-bridge methods*. See SI-5373 for details */
  def isPureBridge(sym: Symbol) = sym.isBridge && sym.allOverriddenSymbols.forall(_.isBridge)

  // the classes that are excluded from the index should also be excluded from the diagrams
  def classExcluded(clazz: TemplateEntity): Boolean = settings.hardcoded.isExcluded(clazz.qualifiedName)

  // the implicit conversions that are excluded from the pages should not appear in the diagram
  def implicitExcluded(convertorMethod: String): Boolean = settings.hardcoded.commonConversionTargets.contains(convertorMethod)

  // whether or not to create a page for an {abstract,alias} type
  def typeShouldDocument(bSym: Symbol, inTpl: DocTemplateImpl) =
    (settings.docExpandAllTypes.value && (bSym.sourceFile != null)) ||
    { val rawComment = global.expandedDocComment(bSym, inTpl.sym)
      rawComment.contains("@template") || rawComment.contains("@documentable") }
}

