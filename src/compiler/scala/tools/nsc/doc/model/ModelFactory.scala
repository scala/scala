/* NSC -- new Scala compiler -- Copyright 2007-2010 LAMP/EPFL */

package scala.tools.nsc
package doc
package model

import comment._

import scala.collection._
import symtab.Flags

/** This trait extracts all required information for documentation from compilation units */
class ModelFactory(val global: Global, val settings: doc.Settings) { extractor =>

  import global._
  import definitions.{ ObjectClass, ScalaObjectClass, RootPackage, EmptyPackage }

  private var droppedPackages = 0
  def templatesCount = templatesCache.size - droppedPackages

  /**  */
  def makeModel: Package =
    makePackage(RootPackage, null) getOrElse { throw new Error("no documentable class found in compilation units") }

  /** */
  protected val commentFactory = new CommentFactory(reporter)

  /** */
  protected val commentCache = mutable.Map.empty[Symbol, comment.Comment]

  /** */
  protected val templatesCache =
    new mutable.LinkedHashMap[(Symbol, TemplateImpl), DocTemplateImpl]

  /* ============== IMPLEMENTATION PROVIDING ENTITY TYPES ============== */

  /** Provides a default implementation for instances of the `Entity` type. */
  abstract class EntityImpl(val sym: Symbol, inTpl: => TemplateImpl) extends Entity {
    val name = sym.nameString
    def inTemplate = inTpl
    def toRoot: List[EntityImpl] = this :: inTpl.toRoot
    def qualifiedName = name
  }

  /** Provides a default implementation for instances of the `WeakTemplateEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  trait TemplateImpl extends EntityImpl with TemplateEntity {
    override def qualifiedName = if (inTemplate.isRootPackage) name else (inTemplate.qualifiedName + "." + name)
    val isPackage = sym.isPackage
    val isTrait = sym.isTrait
    val isClass = sym.isClass && !sym.isTrait
    val isObject = sym.isModule && !sym.isPackage
    val isRootPackage = false
  }

  /** Provides a default implementation for instances of the `WeakTemplateEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  class NoDocTemplateImpl(sym: Symbol, inTpl: => TemplateImpl) extends EntityImpl(sym, inTpl) with TemplateImpl with NoDocTemplate {
    def isDocTemplate = false
  }

  /** Provides a default implementation for instances of the `MemberEntity` type. It must be instantiated as a
    * `SymbolicEntity` to access the compiler symbol that underlies the entity. */
  abstract class MemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends EntityImpl(sym, inTpl) with MemberEntity {
    val comment =
      if (inTpl == null) None else {
        val rawComment = expandedDocComment(sym, inTpl.sym)
        if (rawComment == "") None else {
          val c = commentFactory.parse(rawComment, docCommentPos(sym))
          commentCache += sym -> c
          Some(c)
        }
      }
    override def inTemplate = inTpl
    override def toRoot: List[MemberImpl] = this :: inTpl.toRoot
    def inDefinitionTemplates =
      if (inTpl == null)
        makePackage(RootPackage, null).toList
      else if (sym.owner == inTpl.sym)
        inTpl :: Nil
      else
        makeTemplate(sym.owner) :: (sym.allOverriddenSymbols map { inhSym => makeTemplate(inhSym.owner) })
    val visibility = {
      def qual = {
        val qq =
          if (sym hasFlag Flags.LOCAL)
            Some("this")
          else if (sym.privateWithin != null && sym.privateWithin != NoSymbol)
            Some(sym.privateWithin.nameString) // TODO: create an inline link to the qualifier entity
          else None
        qq match { case Some(q) => "[" + q + "]" case None => "" }
      }
      if (sym hasFlag Flags.PRIVATE) Some(Paragraph(Text("private" + qual)))
      else if (sym hasFlag Flags.PROTECTED) Some(Paragraph(Text("protected" + qual)))
      else None
    }
    val flags = {
      val fgs = mutable.ListBuffer.empty[Paragraph]
      if (sym hasFlag Flags.IMPLICIT) fgs += Paragraph(Text("implicit"))
      if (sym hasFlag Flags.SEALED) fgs += Paragraph(Text("sealed"))
      if (!sym.isTrait && (sym hasFlag Flags.ABSTRACT)) fgs += Paragraph(Text("abstract"))
      if (!sym.isTrait && (sym hasFlag Flags.DEFERRED)) fgs += Paragraph(Text("abstract"))
      if (!sym.isModule && (sym hasFlag Flags.FINAL)) fgs += Paragraph(Text("final"))
      fgs.toList
    }
    lazy val inheritedFrom =
      if (inTemplate.sym == this.sym.owner || inTemplate.sym.isPackage) Nil else
        makeTemplate(this.sym.owner) :: (sym.allOverriddenSymbols map { os => makeTemplate(os.owner) })
    val isDeprecated = sym.isDeprecated
    lazy val resultType = makeType(sym.tpe.finalResultType, inTemplate, sym)
    val isDef = false
    val isVal = false
    val isVar = false
    val isConstructor = false
    val isAliasType = false
    val isAbstractType = false
    val isTemplate = false
  }

  /** Provides a default implementation for instances of the `TemplateEntity` type. It must be instantiated as a
    * `TemplateSymbolicEntity` to access the compiler symbol that underlies the entity and to be registered with the
    * `templatesCache` at the very start of its instantiation.
    *
    * The instantiation of `TemplateImpl` triggers the creation of the following entities.
    * * The owner of the template (as a full template);
    * * All ancestors of the template (as weak templates);
    * * All non-package members (including other templates, as full templates). */
  abstract class DocTemplateImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends MemberImpl(sym, inTpl) with TemplateImpl with DocTemplateEntity {
    //if (inTpl != null) println("mbr " + sym + " in " + (inTpl.toRoot map (_.sym)).mkString(" > "))
    templatesCache += ((sym, inTpl) -> this)
    override def definitionName = inDefinitionTemplates.head.qualifiedName + "." + name
    override def toRoot: List[DocTemplateImpl] = this :: inTpl.toRoot
    val inSource = if (sym.sourceFile != null) Some(sym.sourceFile, sym.pos.line) else None
    val typeParams = if (sym.isClass) sym.typeParams map (makeTypeParam(_, this)) else Nil
    val parentType =
      if (sym.isPackage) None else
        Some(makeType(RefinedType(sym.tpe.parents filter (_ != ScalaObjectClass.tpe), EmptyScope)))
    val linearization = {
      sym.info.parents map { prt =>
        makeTemplate(prt.typeSymbol) match {
          case dtpl: DocTemplateImpl => dtpl.registerSubClass(this)
          case _ =>
        }
      }
      sym.ancestors filter (_ != ScalaObjectClass) map (makeTemplate(_))
    }
    private lazy val subClassesCache = mutable.Buffer.empty[DocTemplateEntity]
    def registerSubClass(sc: DocTemplateEntity) = {
      assert(subClassesCache != null)
      subClassesCache += sc
    }
    def subClasses = subClassesCache.toList
    def memberSyms = sym.info.nonPrivateMembers
    val members: List[MemberEntity] = memberSyms flatMap (makeMember(_, this))
    val templates     = members partialMap { case c: DocTemplateEntity => c }
    val methods       = members partialMap { case d: Def => d }
    val values        = members partialMap { case v: Val => v }
    val abstractTypes = members partialMap { case t: AbstractType => t }
    val aliasTypes    = members partialMap { case t: AliasType => t }
    override val isTemplate = true
    def isDocTemplate = true
  }

  abstract class PackageImpl(sym: Symbol, inTpl: => PackageImpl) extends DocTemplateImpl(sym, inTpl) with Package {
    override def inTemplate = inTpl
    override def toRoot: List[PackageImpl] = this :: inTpl.toRoot
    val packages = members partialMap { case p: Package => p }
  }

  abstract class NonTemplateMemberImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends MemberImpl(sym, inTpl) with NonTemplateMemberEntity {
    override def qualifiedName = inTemplate.qualifiedName + "#" + name
    override def definitionName = inDefinitionTemplates.head.qualifiedName + "#" + name
  }

  abstract class ParameterImpl(sym: Symbol, inTpl: => DocTemplateImpl) extends EntityImpl(sym, inTpl) with ParameterEntity {
    override def inTemplate = inTpl
  }

  /* ============== MAKER METHODS ============== */

  /** */
  def normalizeTemplate(aSym: Symbol): Symbol = {
    if (aSym == null || aSym == EmptyPackage || aSym == NoSymbol)
      normalizeTemplate(RootPackage)
    else if (aSym == ScalaObjectClass || aSym == ObjectClass)
      normalizeTemplate(definitions.AnyRefClass)
    else if (aSym.isModuleClass || aSym.isPackageObject)
      normalizeTemplate(aSym.sourceModule)
    else
      aSym
  }

  /** Creates a package entity for the given symbol or returns `None` if the symbol does not denote a package that
    * contains at least one ''documentable'' class, trait or object. Creating a package entity */
  def makePackage(aSym: Symbol, inTpl: => PackageImpl): Option[PackageImpl] = {
    val bSym = normalizeTemplate(aSym)
    if (templatesCache isDefinedAt (bSym, inTpl))
      Some(templatesCache(bSym, inTpl) match {case p: PackageImpl => p})
    else {
      val pack =
        if (bSym == RootPackage)
          new PackageImpl(bSym, null) {
            override val name = "root"
            override def inTemplate = this
            override def toRoot = this :: Nil
            override def qualifiedName = "_root_"
            override lazy val inheritedFrom = Nil
            override val isRootPackage = true
            override def memberSyms =
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
      makePackage(bSym, null).get
    else
      makeTemplate(bSym, makeTemplate(bSym.owner))
  }

  /** */
  def makeTemplate(aSym: Symbol, inTpl: => TemplateImpl): TemplateImpl = {
    val bSym = normalizeTemplate(aSym)
    if (bSym.isPackage) inTpl match {
      case inPkg: PackageImpl => makePackage(bSym, inPkg) getOrElse (new NoDocTemplateImpl(bSym, inPkg))
      case _ => throw new Error("'" + bSym + "' must be in a package")
    }
    else if ((bSym.sourceFile != null) && bSym.isPublic && !bSym.isLocal) inTpl match {
      case inDTpl: DocTemplateImpl => makeDocTemplate(bSym, inDTpl)
      case _ => new NoDocTemplateImpl(bSym, inTpl) // The owner is private
    }
    else
      new NoDocTemplateImpl(bSym, inTpl)
  }

  /** */
  def makeDocTemplate(aSym: Symbol, inTpl: => DocTemplateImpl): DocTemplateImpl = {
    val bSym = normalizeTemplate(aSym)
    val firstInTpl = { // to prevent a complexity explosion in some cases.
      def sInTpl0(inTpl: DocTemplateImpl): DocTemplateImpl =
        if ((aSym.owner != inTpl.inTemplate.sym) && (inTpl.inTemplate.sym.info.members contains aSym))
          sInTpl0(inTpl.inTemplate)
        else inTpl
      sInTpl0(inTpl)
    }
    if (templatesCache isDefinedAt (bSym, firstInTpl))
      templatesCache((bSym, firstInTpl))
    else if (bSym.isModule || (bSym.isAliasType && bSym.tpe.typeSymbol.isModule))
      new DocTemplateImpl(bSym, firstInTpl) with Object
    else if (bSym.isTrait || (bSym.isAliasType && bSym.tpe.typeSymbol.isTrait))
      new DocTemplateImpl(bSym, firstInTpl) with Trait {
        val valueParams =
          List(sym.constrParamAccessors map (makeValueParam(_, this)))
      }
    else if (bSym.isClass || (bSym.isAliasType && bSym.tpe.typeSymbol.isClass))
      new DocTemplateImpl(bSym, firstInTpl) with Class {
        val valueParams =
          List(sym.constrParamAccessors map (makeValueParam(_, this)))
        val constructors =
          members partialMap { case d: Constructor => d }
        val primaryConstructor = (constructors find (_.isPrimary))
        val isCaseClass = sym.isClass && sym.hasFlag(Flags.CASE)
      }
    else
      throw new Error("'" + bSym + "' that isn't a class, trait or object cannot be built as a documentable template")
  }

  /** */
  def makeMember(aSym: Symbol, inTpl: => DocTemplateImpl): Option[MemberImpl] = {
    if (!aSym.isPublic || (aSym hasFlag Flags.SYNTHETIC) || (aSym hasFlag Flags.BRIDGE) || aSym.isLocal || aSym.isModuleClass || aSym.isPackageObject || aSym.isMixinConstructor)
      None
    else if (aSym.isGetter && (aSym.accessed hasFlag Flags.MUTABLE))
      Some(new NonTemplateMemberImpl(aSym, inTpl) with Val {
        override val isVar = true
      })
    else if (aSym.isMethod && !(aSym hasFlag Flags.ACCESSOR) && !aSym.isConstructor && !(aSym hasFlag Flags.FINAL))
      Some(new NonTemplateMemberImpl(aSym, inTpl) with Def {
        override val isDef = true
        val typeParams =
          sym.tpe.typeParams map (makeTypeParam(_, inTpl))
        val valueParams =
          sym.paramss map { ps => (ps.zipWithIndex) map { case (p, i) =>
            if (p.nameString contains "$") makeValueParam(p, inTpl, "arg" + i) else makeValueParam(p, inTpl)
          }}
      })
    else if (aSym.isConstructor)
      Some(new NonTemplateMemberImpl(aSym, inTpl) with Constructor {
        override val isConstructor = true
        val isPrimary = sym.isPrimaryConstructor
        val valueParams =
          sym.paramss map { ps => (ps.zipWithIndex) map { case (p, i) =>
            if (p.nameString contains "$") makeValueParam(p, inTpl, "arg" + i) else makeValueParam(p, inTpl)
          }}
      })
    else if (aSym.isGetter) // Scala field accessor or Java field
      Some(new NonTemplateMemberImpl(aSym, inTpl) with Val {
        override val isVal = true
      })
    else if (aSym.isAbstractType)
      Some(new NonTemplateMemberImpl(aSym, inTpl) with AbstractType {
        override val isAbstractType = true
        val lo = sym.info.normalize match {
          case TypeBounds(lo, hi) if lo.typeSymbol != definitions.NothingClass => Some(makeType(lo, inTpl, sym))
          case _ => None
        }
        val hi = sym.info.normalize match {
          case TypeBounds(lo, hi) if hi.typeSymbol != definitions.AnyClass => Some(makeType(hi, inTpl, sym))
          case _ => None
        }
      })
    else if (aSym.isAliasType)
      Some(new NonTemplateMemberImpl(aSym, inTpl) with AliasType {
        override val isAliasType = true
        val alias = makeType(sym.tpe, inTpl, sym)
      })
    else if (aSym.isPackage)
      inTpl match { case inPkg: PackageImpl =>  makePackage(aSym, inPkg) }
    else if ((aSym.isClass || aSym.isModule) && (aSym.sourceFile != null) && aSym.isPublic && !aSym.isLocal) {
      (inTpl.toRoot find (_.sym == aSym )) orElse Some(makeDocTemplate(aSym, inTpl))
    }
    else
      None
  }

  /** */
  def makeTypeParam(aSym: Symbol, inTpl: => DocTemplateImpl): TypeParam = {
    new ParameterImpl(aSym, inTpl) with TypeParam {
      val isTypeParam = true
      val isValueParam = false
      val variance: String = {
        if (sym hasFlag Flags.COVARIANT) "+"
        else if (sym hasFlag Flags.CONTRAVARIANT) "-"
        else ""
      }
      val lo = sym.info.normalize match {
        case TypeBounds(lo, hi) if lo.typeSymbol != definitions.NothingClass =>
          Some(makeType(lo, inTpl, sym))
        case _ => None
      }
      val hi = sym.info.normalize match {
        case TypeBounds(lo, hi) if hi.typeSymbol != definitions.AnyClass =>
          Some(makeType(hi, inTpl, sym))
        case _ => None
      }
    }
  }

  /** */
  def makeValueParam(aSym: Symbol, inTpl: => DocTemplateImpl): ValueParam = {
    makeValueParam(aSym, inTpl, aSym.nameString)
  }

  /** */
  def makeValueParam(aSym: Symbol, inTpl: => DocTemplateImpl, newName: String): ValueParam = {
    new ParameterImpl(aSym, inTpl) with ValueParam {
      val isTypeParam = false
      val isValueParam = true
      val resultType = {
        makeType(sym.tpe, inTpl, sym)
      }
      override val name = newName
    }
  }

  /** */
  def makeType(aType: Type, seeInTpl: => TemplateImpl, dclSym: Symbol): TypeEntity = {
    def ownerTpl(sym: Symbol): Symbol =
      if (sym.isClass || sym.isModule || sym == NoSymbol) sym else ownerTpl(sym.owner)
    makeType(aType.asSeenFrom(seeInTpl.sym.thisType, ownerTpl(dclSym)))
  }

  /** */
  def makeType(aType: Type): TypeEntity =
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
      private def appendType0(tpe: Type): Unit = tpe.normalize match {
        /* Type refs */
        case tp: TypeRef if (definitions.isFunctionType(tp)) =>
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
        case tp: TypeRef if (definitions.isTupleType(tp)) =>
          nameBuffer append '('
          appendTypes0(tp.args, ", ")
          nameBuffer append ')'
        case TypeRef(pre, aSym, targs) =>
          val bSym = normalizeTemplate(aSym)
          if (bSym.isTypeMember)
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
        case PolyType(tparams, result) if (!tparams.isEmpty) =>
          appendType0(result)
          nameBuffer append '['
          appendTypes0(tparams map (_.tpe), ", ") // TODO: actually print the polytype's symbols (not just types)
          nameBuffer append ']'
        /* Eval-by-name types */
        case PolyType(tparams, result) if (tparams.isEmpty) =>
          nameBuffer append '⇒'
          appendType0(result)
        case tpen =>
          nameBuffer append tpen.toString
      }
      appendType0(aType)
      val refEntity = refBuffer
      val name = nameBuffer.toString
    }

}
