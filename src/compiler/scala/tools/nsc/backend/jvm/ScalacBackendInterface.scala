package scala.tools.nsc.backend.jvm

import scala.collection.generic.Clearable
import scala.collection.mutable
import scala.reflect.ClassTag
import scala.reflect.internal.util.WeakHashSet
import scala.tools.nsc.Global
import scala.tools.asm
import scala.reflect.io.AbstractFile
import scala.language.implicitConversions
import scala.reflect.internal.{Flags => IFlags}


/* Uses pre-allocated objects to not allocate objects during pattern matching and during implicit conversion.
 * Due to this every instance in not threadsafe, but multiple instances could be used in parrallel if underlying compiler
 * supports this
 */

class ScalacBackendInterface[G <: Global](val global: G) extends BackendInterface with BackendInterfaceDefinitions{
  import global._
  import definitions._

  class NonExistentTree extends global.Tree {
    def productElement(n: Int): Any = ???
    def productArity: Int = ???
    def canEqual(that: Any): Boolean = ???
  }

  type Symbol          = global.Symbol
  type Type            = global.Type
  type Annotation      = global.AnnotationInfo
  type Tree            = global.Tree
  type CompilationUnit = global.CompilationUnit
  type Constant        = global.Constant
  type Literal         = global.Literal
  type Position        = global.Position
  type Name            = global.Name
  type LabelDef        = global.LabelDef
  type ClassDef        = global.ClassDef
  type TypeDef         = global.TypeDef
  type Apply           = global.Apply
  type TypeApply       = global.TypeApply
  type Try             = global.Try
  type Assign          = global.Assign
  type Ident           = global.Ident
  type If              = global.If
  type ValDef          = global.ValDef
  type Throw           = global.Throw
  type Return          = global.Return
  type Block           = global.Block
  type Typed           = global.Typed
  type ArrayValue      = global.ArrayValue
  type Match           = global.Match
  type This            = global.This
  type CaseDef         = global.CaseDef
  type Alternative     = global.Alternative
  type DefDef          = global.DefDef
  type ModuleDef       = global.ModuleDef
  type Template        = global.Template
  type Select          = global.Select
  type Bind            = global.Bind
  type New             = global.New
  type ApplyDynamic    = global.ApplyDynamic
  type Super           = global.Super
  type Modifiers       = global.Modifiers
  type Closure         = NonExistentTree

  val NoSymbol = global.NoSymbol
  val NoPosition: Position = global.NoPosition
  val EmptyTree: Tree = global.EmptyTree

  import scala.tools.nsc.symtab._

  def currentUnit: CompilationUnit = global.currentUnit

  implicit def symHelper(s: Symbol): SymbolHelper = {
    ScalacSymbolHelper.sym = s
    ScalacSymbolHelper
  }

  implicit def typeHelper(tp: Type): TypeHelper = {
    ScalaCTypeHelper.t = tp
    ScalaCTypeHelper
  }

  implicit def nameHelper(nm: Name): NameHelper = {
    ScalacNameHelper.n = nm
    ScalacNameHelper
  }

  implicit def annotHelper(a: Annotation): AnnotationHelper = {
    ScalacAnnotationHelper.t = a
    ScalacAnnotationHelper
  }

  implicit def treeHelper(a: Tree): TreeHelper = {
    ScalacTreeHelper.t = a
    ScalacTreeHelper
  }

  implicit def constantHelper(a: Constant): ConstantHelper = {
    ScalacConstantHelper.c = a
    ScalacConstantHelper
  }

  implicit def positionHelper(a: Position): PositionHelper = {
    ScalacPositionHelper.p = a
    ScalacPositionHelper
  }

  val UnitTag: ConstantTag = global.UnitTag
  val IntTag: ConstantTag = global.IntTag
  val FloatTag: ConstantTag = global.FloatTag
  val NullTag: ConstantTag = global.NullTag
  val BooleanTag: ConstantTag = global.BooleanTag
  val ByteTag: ConstantTag = global.ByteTag
  val ShortTag: ConstantTag = global.ShortTag
  val CharTag: ConstantTag = global.CharTag
  val DoubleTag: ConstantTag = global.DoubleTag
  val LongTag: ConstantTag = global.LongTag
  val StringTag: ConstantTag = global.StringTag
  val ClazzTag: ConstantTag = global.ClazzTag
  val EnumTag: ConstantTag = global.EnumTag

  val hashMethodSym: Symbol = getMember(ScalaRunTimeModule, nme.hash_)


  val String_valueOf: Symbol = getMember(StringModule, nme.valueOf).filter(sym => sym.info.paramTypes match {
    case List(pt) => pt.typeSymbol == ObjectClass
    case _        => false
  })

  val UnitClass: Symbol = global.definitions.UnitClass
  val BooleanClass: Symbol = global.definitions.BooleanClass
  val CharClass: Symbol  = global.definitions.CharClass
  val ShortClass: Symbol = global.definitions.ShortClass
  val ClassClass: Symbol = global.definitions.ClassClass
  val ByteClass: Symbol  = global.definitions.ByteClass
  val IntClass: Symbol = global.definitions.IntClass
  val LongClass: Symbol = global.definitions.LongClass
  val FloatClass: Symbol = global.definitions.FloatClass
  val DoubleClass: Symbol  = global.definitions.DoubleClass


  val ArrayClass: Symbol = global.definitions.ArrayClass
  val NothingClass: Symbol = global.definitions.NothingClass
  val NullClass: Symbol = global.definitions.NullClass
  val ObjectClass: Symbol = global.definitions.ObjectClass
  val Object_Type: Type = global.definitions.ObjectTpe
  val Throwable_Type: Type = global.definitions.ThrowableTpe
  val Object_isInstanceOf: Symbol = global.definitions.Object_isInstanceOf
  val Object_asInstanceOf: Symbol = global.definitions.Object_asInstanceOf
  val Object_equals: Symbol = global.definitions.Object_equals
  val Array_clone: Symbol = global.definitions.Array_clone
  lazy val externalEqualsNumNum: Symbol = platform.externalEqualsNumNum
  lazy val externalEqualsNumChar: Symbol = platform.externalEqualsNumChar
  lazy val externalEqualsNumObject: Symbol = platform.externalEqualsNumObject
  lazy val externalEquals: Symbol = platform.externalEquals
  val MaxFunctionArity: Int = global.definitions.MaxFunctionArity
  val FunctionClass: Array[Symbol] = global.definitions.FunctionClass.seq.toArray
  val AbstractFunctionClass: Array[Symbol] = global.definitions.AbstractFunctionClass.seq.toArray
  val PartialFunctionClass: Symbol = global.definitions.PartialFunctionClass
  val AbstractPartialFunctionClass: Symbol = global.definitions.AbstractPartialFunctionClass


  object Assign extends AssignDeconstructor {
    def _1: Tree = field.lhs
    def _2: Tree = field.rhs
  }

  object Select extends SelectDeconstructor {
    def _1: Tree = field.qualifier
    def _2: Name = field.name
  }

  object Apply extends ApplyDeconstructor {
    def _1: Tree = field.fun
    def _2: List[Tree] = field.args
  }

  object If extends IfDeconstructor {
    def _1: Tree = field.cond
    def _2: Tree = field.thenp
    def _3: Tree = field.elsep
  }

  object ValDef extends ValDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _3: Tree = field.tpt
    def _4: Tree = field.rhs
  }

  object ApplyDynamic extends ApplyDynamicDeconstructor {
    def _1: Tree = field.qual
    def _2: List[Tree] = field.args
  }

  object Literal extends LiteralDeconstructor {
    def get = field.value
  }

  object Throw extends ThrowDeconstructor {
    def get = field.expr
  }

  object New extends NewDeconstructor {
    def get = field.tpt.tpe
  }

  object This extends ThisDeconstructor {
    def get = field.qual
    def apply(s: global.Symbol): This = global.This(s.name.toTypeName) setSymbol s
  }

  object Return extends ReturnDeconstructor {
    def get = field.expr
  }

  object Ident extends IdentDeconstructor {
    def get = field.name
  }

  object Alternative extends AlternativeDeconstructor {
    def get = field.trees
  }
  object Constant extends ConstantDeconstructor {
    def get = field.value
  }
  object ThrownException extends ThrownException {
    def unapply(a: Annotation): Option[Symbol] = global.ThrownException.unapply(a)
  }

  object Try extends TryDeconstructor {
    def _1: Tree = field.block
    def _2: List[Tree] = field.catches
    def _3: Tree = field.finalizer
  }

  object LabelDef extends LabelDeconstructor {
    def _1: Name = field.name
    def _2: List[Symbol] = field.params.map(_.symbol)
    def _3: Tree = field.rhs
  }

  object Typed extends TypedDeconstrutor {
    def _1: Tree = field.expr
    def _2: Tree = field.tpt
  }
  object Super extends SuperDeconstructor {
    def _1: Tree = field.qual
    def _2: Name = field.mix
  }
  object ArrayValue extends ArrayValueDeconstructor {
    def _1: Type = field.elemtpt.tpe
    def _2: List[Tree] = field.elems
  }
  object Match extends MatchDeconstructor {
    def _1: Tree = field.selector
    def _2: List[Tree] = field.cases
  }
  object Block extends BlockDeconstructor {
    def _1: List[Tree] = field.stats
    def _2: Tree = field.expr
  }
  object TypeApply extends TypeApplyDeconstructor {
    def _1: Tree = field.fun
    def _2: List[Tree] = field.args
  }
  object CaseDef extends CaseDeconstructor {
    def _1: Tree = field.pat
    def _2: Tree = field.guard
    def _3: Tree = field.body
  }

  object DefDef extends DefDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _3: List[TypeDef] = field.tparams
    def _4: List[List[ValDef]] = field.vparamss
    def _5: Tree = field.tpt
    def _6: Tree = field.rhs
  }

  object ModuleDef extends ModuleDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _3: Tree = field.impl
  }

  object Template extends TemplateDeconstructor {
    def _1: List[Tree] = field.parents
    def _2: ValDef = field.self
    def _3: List[Tree] = field.body
  }

  object Bind extends BindDeconstructor {
    def _1: Name = field.name
    def _2: Tree = field.body
  }

  object ClassDef extends ClassDefDeconstructor {
    def _1: Modifiers = field.mods
    def _2: Name = field.name
    def _4: Template = field.impl
    def _3: List[TypeDef] = field.tparams
  }

  object Closure extends ClosureDeconstructor {
    def _1 = ???
    def _2 = ???
    def _3 = ???
    override def isEmpty = true
  }

  object ScalacPrimitives extends Primitives {
    def getPrimitive(app: Apply, reciever: Type): Int = global.scalaPrimitives.getPrimitive(app.symbol, reciever)

    def getPrimitive(sym: Symbol): Int = global.scalaPrimitives.getPrimitive(sym)

    def isPrimitive(sym: Symbol): Boolean = global.scalaPrimitives.isPrimitive(sym)
  }

  val primitives: Primitives = ScalacPrimitives

  val nme_This: Name = nme.This
  val nme_EMPTY_PACKAGE_NAME: Name = nme.EMPTY_PACKAGE_NAME
  val nme_CONSTRUCTOR: Name = nme.CONSTRUCTOR
  val nme_WILDCARD: Name = nme.WILDCARD
  val nme_THIS: Name = nme.THIS
  val nme_PACKAGE: Name = nme.PACKAGE
  val nme_EQEQ_LOCAL_VAR: Name = nme.EQEQ_LOCAL_VAR
  val nme_valueOf: Name = nme.valueOf
  val nme_apply: Name = nme.apply


  val Flag_METHOD: Flags = IFlags.METHOD
  val Flag_SYNTHETIC: Flags = IFlags.SYNTHETIC


  def debuglog(msg: => String): Unit = global.debuglog(msg)
  def error(pos: Position, msg: String): Unit = global.reporter.error(pos, msg)
  def warning(pos: Position, msg: String): Unit = global.reporter.warning(pos, msg)
  def abort(msg: String): Nothing = global.abort(msg)
  def debuglevel: Int = settings.debuginfo.indexOfChoice
  def settings_debug: Boolean = settings.debug



  /* means of getting class symbols from compiler */
  def requiredClass[T: ClassTag]: Symbol = rootMirror.requiredClass(implicitly[ClassTag[T]])
  def requiredModule[T: ClassTag]: Symbol = rootMirror.requiredModule(implicitly[ClassTag[T]])

  def getRequiredClass(fullname: String): Symbol = rootMirror.getRequiredClass(fullname)

  def getClassIfDefined(fullname: String): Symbol = rootMirror.getClassIfDefined(fullname)

  def shouldEmitJumpAfterLabels: Boolean = false

  def shouldEmitAnnotation(annot: Annotation): Boolean = {
    annot.symbol.initialize.isJavaDefined &&
      annot.matches(ClassfileAnnotationClass) &&
      retentionPolicyOf(annot) != AnnotationRetentionPolicySourceValue &&
      annot.args.isEmpty
  }

  def isRuntimeVisible(annot: Annotation): Boolean = {
    annot.atp.typeSymbol.getAnnotation(AnnotationRetentionAttr) match {
      case Some(retentionAnnot) =>
        retentionAnnot.assocs.contains(nme.value -> LiteralAnnotArg(new Constant(AnnotationRetentionPolicyRuntimeValue)))
      case _ =>
        // SI-8926: if the annotation class symbol doesn't have a @RetentionPolicy annotation, the
        // annotation is emitted with visibility `RUNTIME`
        true
    }
  }

  private def retentionPolicyOf(annot: AnnotationInfo): Symbol =
    annot.atp.typeSymbol.getAnnotation(AnnotationRetentionAttr).map(_.assocs).map(assoc =>
      assoc.collectFirst {
        case (`nme`.value, LiteralAnnotArg(Constant(value: Symbol))) => value
      }).flatten.getOrElse(AnnotationRetentionPolicyClassValue)

  lazy val AnnotationRetentionPolicyModule       = AnnotationRetentionPolicyAttr.companionModule
  lazy val AnnotationRetentionPolicySourceValue  = AnnotationRetentionPolicyModule.tpe.member(TermName("SOURCE"))
  lazy val AnnotationRetentionPolicyClassValue   = AnnotationRetentionPolicyModule.tpe.member(TermName("CLASS"))
  lazy val AnnotationRetentionPolicyRuntimeValue = AnnotationRetentionPolicyModule.tpe.member(TermName("RUNTIME"))

  def informProgress(msg: String): Unit = if (settings.verbose) inform("[" + msg + "]")

  val ExcludedForwarderFlags: Flags = {
    import scala.tools.nsc.symtab.Flags._
    // Should include DEFERRED but this breaks findMember.
    SPECIALIZED | LIFTED | PROTECTED | STATIC | EXPANDEDNAME | BridgeAndPrivateFlags | MACRO
  }

  val MODULE_INSTANCE_FIELD = nme.MODULE_INSTANCE_FIELD.toString
  def internalNameString(offset: Int, length: Int) = new String(global.chrs, offset, length)


  def targetPlatform: String = settings.target.value

  def setMainClass(name: String): Unit = settings.mainClass.value = name

  override def emitAsmp = if (settings.Ygenasmp.isSetByUser) Some(settings.Ygenasmp.value) else None

  override def dumpClasses: Option[String] = if(settings.Ydumpclasses.isSetByUser) Some(settings.Ydumpclasses.value) else None

  override def mainClass = {
    if (settings.mainClass.isDefault) None
    else Some(settings.mainClass.value)
  }

  object scalacCaches extends Caches{
    def recordCache[T <: Clearable](cache: T): T = global.perRunCaches.recordCache(cache)
    def newAnyRefMap[K <: AnyRef, V](): mutable.AnyRefMap[K, V] = global.perRunCaches.newAnyRefMap()
    def newWeakMap[K, V](): mutable.WeakHashMap[K, V] = global.perRunCaches.newWeakMap()
    def newWeakSet[K <: AnyRef](): WeakHashSet[K] = global.perRunCaches.newWeakSet()
    def newMap[K, V](): mutable.HashMap[K, V] = global.perRunCaches.newMap()
    def newSet[K](): mutable.Set[K] = global.perRunCaches.newSet()
  }

  def perRunCaches: Caches = scalacCaches


  /* backend actually uses free names to generate stuff. This should NOT mangled */
  def newTermName(prefix: String): Name = global.newTermName(prefix)

  def isMaybeBoxed(sym: Symbol): Boolean = global.platform.isMaybeBoxed(sym)

  def getSingleOutput: Option[AbstractFile] = settings.outputDirs.getSingleOutput


  override def boxMethods = currentRun.runDefinitions.boxMethod

  // (class, method)
  override def unboxMethods = currentRun.runDefinitions.unboxMethod

  object ScalacSymbolHelper extends SymbolHelper {
    var sym: Symbol = _

     // names
    def fullName(sep: Char): String = sym.fullName(sep)
    def fullName: String = sym.fullName
    def simpleName: Name = sym.simpleName
    def javaSimpleName: Name = sym.javaSimpleName
    def javaBinaryName: Name = sym.javaBinaryName
    def javaClassName: String = sym.javaClassName
    def name: Name = sym.name
    def rawname: Name = sym.rawname
    def nestedClasses: List[Symbol] = exitingPhase(currentRun.lambdaliftPhase)(sym.memberClasses)



    // types
    def info: Type = sym.info
    def tpe: Type = sym.tpe
    def thisType: Type = sym.thisType




    // tests
    def isClass: Boolean = sym.isClass
    def isType: Boolean = sym.isType
    def isAnonymousClass: Boolean = sym.isAnonymousClass
    def isConstructor: Boolean = sym.isConstructor
    def isAnonymousFunction: Boolean = sym.isAnonymousFunction
    def isMethod: Boolean = sym.isMethod
    def isPublic: Boolean = sym.isPublic
    def isSynthetic: Boolean = sym.isSynthetic
    def isPackageClass: Boolean = sym.isPackageClass
    def isModuleClass: Boolean = sym.isModuleClass
    def isModule: Boolean = sym.isModule
    def isStrictFP: Boolean = sym.isStrictFP
    def isLabel: Boolean = sym.isLabel
    def hasPackageFlag: Boolean = sym.hasPackageFlag
    def isImplClass: Boolean = sym.isImplClass
    def isInterface: Boolean = sym.isInterface
    def hasGetter: Boolean = sym.hasGetter
    def isGetter: Boolean = sym.isGetter
    def isSetter: Boolean = sym.isSetter
    def isJavaDefined: Boolean = sym.isJavaDefined
    def isJavaDefaultMethod: Boolean = false
    def isDeferred: Boolean = sym.isDeferred
    def isStaticMember: Boolean = sym.isStaticMember
    def isBottomClass: Boolean = sym.isBottomClass
    def isBridge: Boolean = sym.isBridge
    def isArtifact: Boolean = sym.isArtifact
    def hasEnumFlag: Boolean = sym.hasEnumFlag
    def hasAccessBoundary: Boolean = sym.hasAccessBoundary
    def isVarargsMethod: Boolean = sym.isVarargsMethod
    def isDeprecated: Boolean = sym.isDeprecated
    def isMutable: Boolean = sym.isMutable
    def hasAbstractFlag: Boolean = sym.hasAbstractFlag
    def hasModuleFlag: Boolean = sym.hasModuleFlag
    def isNonBottomSubClass(sym2: Symbol): Boolean = sym.isNonBottomSubClass(sym2)
    def isGetClass: Boolean = definitions.isGetClass(sym)
    def hasAnnotation(sym2: Symbol): Boolean = sym.hasAnnotation(sym2)
    def isClassConstructor: Boolean = sym.isClassConstructor
    def isStaticConstructor: Boolean = sym.isStaticConstructor



    // navigation
    def owner: Symbol = sym.owner
    def rawowner: Symbol = sym.rawowner
    def originalOwner: Symbol = sym.originalOwner
    def parentSymbols: List[Symbol] = sym.parentSymbols
    def superClass: Symbol = sym.superClass
    def enclClass: Symbol = sym.enclClass
    def linkedClassOfClass: Symbol = sym.linkedClassOfClass
    def companionClass: Symbol = sym.companionClass
    def companionModule: Symbol = sym.companionModule
    def companionSymbol: Symbol = sym.companionSymbol
    def moduleClass: Symbol = sym.moduleClass
    def primaryConstructor: Symbol = sym.primaryConstructor


    def annotations: List[Annotation] = sym.annotations

    def moduleSuffix: String = sym.moduleSuffix

    def getter(clz: Symbol): Symbol = sym.getterIn(clz)

    def outputDirectory: AbstractFile = settings.outputDirs outputDirFor enteringFlatten(sym.sourceFile)

    def freshLocal(cunit: CompilationUnit, name: String, tpe: Type, pos: Position, flags: Flags): Symbol = {
      sym.newVariable(cunit.freshTermName(name), pos, flags).setInfo(tpe)
    }

    def setter(clz: Symbol): Symbol = sym.setterIn(clz)

    def serialVUID: Option[Long] = sym getAnnotation definitions.SerialVersionUIDAttr collect {
      case AnnotationInfo(_, _, (_, LiteralAnnotArg(const)) :: Nil) => const.longValue
    }

    def pos: Position = sym.pos

    def throwsAnnotations: List[Symbol] = sym.throwsAnnotations()

    /**
     * The member classes of a class symbol. Note that the result of this method depends on the
     * current phase, for example, after lambdalift, all local classes become member of the enclosing
     * class.
     */
    def memberClasses: List[Symbol] = sym.info.decls.collect({
      case sym if sym.isClass =>
        sym
      case sym if sym.isModule =>
        val r = exitingPickler(sym.moduleClass)
        assert(r != NoSymbol, sym.fullLocationString)
        r
    })(collection.breakOut)


    def addRemoteRemoteExceptionAnnotation: Unit = {
      val c   = new Constant(RemoteExceptionClass.tpe)
      val arg = new Literal(c) setType c.tpe
      sym.addAnnotation(appliedType(definitions.ThrowsClass, c.tpe), arg)
    }

    def linkedClass: Symbol = exitingPickler(sym.linkedClassOfClass) // linkedCoC does not work properly in late phases
    def companionModuleMembers: List[Symbol] = {
      // phase travel to exitingPickler: this makes sure that memberClassesOf only sees member classes,
      // not local classes of the companion module (E in the exmaple) that were lifted by lambdalift.
      if (linkedClass.isTopLevelModuleClass) exitingPickler(linkedClass.memberClasses)
      else Nil
    }

    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     *
     */
     def superInterfaces: List[Symbol] =  {

      // Additional interface parents based on annotations and other cues
      def newParentForAnnotation(ann: AnnotationInfo): Symbol =
        if (ann.symbol eq RemoteAttr) RemoteInterfaceClass
        else NoSymbol

      val superInterfaces0: List[Symbol] = sym.mixinClasses
      val superInterfaces = existingSymbols(superInterfaces0 ++ sym.annotations.map(newParentForAnnotation)).distinct

      assert(!superInterfaces.contains(NoSymbol), s"found NoSymbol among: ${superInterfaces.mkString(", ")}")
      assert(superInterfaces.forall(s => s.isInterface || s.isTrait), s"found non-interface among: ${superInterfaces.mkString(", ")}")

      erasure.minimizeInterfaces(superInterfaces.map(_.info)).map(_.typeSymbol)
    }
    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean = exitingPickler {
      // phase travel to pickler required for isNestedClass (looks at owner)
      val r = sym.isModuleClass && !sym.isNestedClass
      // The mixin phase adds the `lateMODULE` flag to trait implementation classes. Since the flag
      // is late, it should not be visible here inside the time travel. We check this.
      if (r) assert(!sym.isImplClass, s"isModuleClass should be false for impl class $sym")
      r
    }

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean = {
      /* (1) Phase travel to to pickler is required to exclude implementation classes; they have the
       * lateMODULEs after mixin, so isModuleClass would be true.
       * (2) isStaticModuleClass is a source-level property. See comment on isOriginallyStaticOwner.
       */
      exitingPickler { // (1)
        sym.isModuleClass &&
          sym.originalOwner.isOriginallyStaticOwner // (2)
      }
    }

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean = {
      sym.isPackageClass || sym.isModuleClass && sym.originalOwner.isOriginallyStaticOwner
    }

     def isSynchronized: Boolean = sym.hasFlag(Flags.SYNCHRONIZED)

     def enclosingClassSym: Symbol =  {
      if (sym.isJavaDefined && sym.rawowner.isModuleClass) {
        // Example java source: class C { static class D { } }
        // The Scala compiler creates a class and a module symbol for C. Because D is a static
        // nested class, the symbol for D is nested in the module class C (not in the class C).
        // For the InnerClass attribute, we use the class symbol C, which represents the situation
        // in the source code.

        // Cannot use innerClassSym.isStatic: this method looks at the owner, which is a package
        // at this pahse (after lambdalift, flatten).
        assert(sym.originalOwner.isOriginallyStaticOwner, sym.originalOwner)

        // phase travel for linkedCoC - does not always work in late phases
        exitingPickler(sym.rawowner.linkedClassOfClass)
      }
      else sym.rawowner
    }

    def fieldSymbols: List[Symbol] = {
      for (f <- sym.info.decls.toList ;
           if !f.isMethod && f.isTerm && !f.isModule
      ) yield f
    }


    def methodSymbols: List[Symbol] = {
      // cd.impl.body collect { case dd: DefDef => dd.symbol }
      for (f <- sym.info.decls.toList ;
           if f.isMethod
      ) yield f
    }


    def shouldEmitForwarders: Boolean = {
      exitingPickler { !(sym.name.toString contains '$') && sym.hasModuleFlag && !sym.isImplClass && !sym.isNestedClass }
    }

    def isPrivate = sym.isPrivate

    def getsJavaPrivateFlag: Boolean = {
      // constructors of module classes should be private. introduced in b06edbc, probably to prevent
      // creating module instances from java. for nested modules, the constructor needs to be public
      // since they are created by the outer class and stored in a field. a java client can create
      // new instances via outerClassInstance.new InnerModuleClass$().
      // TODO: do this early, mark the symbol private.
      sym.isPrivate || (sym.isPrimaryConstructor && sym.owner.isTopLevelModuleClass)
    }

    def isFinal = sym.isFinal

    def getsJavaFinalFlag: Boolean = {
      // Symbols marked in source as `final` have the FINAL flag. (In the past, the flag was also
      // added to modules and module classes, not anymore since 296b706).
      // Note that the presence of the `FINAL` flag on a symbol does not correspond 1:1 to emitting
      // ACC_FINAL in bytecode.
      //
      // Top-level modules are marked ACC_FINAL in bytecode (even without the FINAL flag). Nested
      // objects don't get the flag to allow overriding (under -Yoverride-objects, SI-5676).
      //
      // For fields, only eager val fields can receive ACC_FINAL. vars or lazy vals can't:
      // Source: http://docs.oracle.com/javase/specs/jls/se7/html/jls-17.html#jls-17.5.3
      // "Another problem is that the specification allows aggressive
      // optimization of final fields. Within a thread, it is permissible to
      // reorder reads of a final field with those modifications of a final
      // field that do not take place in the constructor."
      //
      // A var or lazy val which is marked final still has meaning to the
      // scala compiler. The word final is heavily overloaded unfortunately;
      // for us it means "not overridable". At present you can't override
      // vars regardless; this may change.
      //
      // The logic does not check .isFinal (which checks flags for the FINAL flag,
      // and includes symbols marked lateFINAL) instead inspecting rawflags so
      // we can exclude lateFINAL. Such symbols are eligible for inlining, but to
      // avoid breaking proxy software which depends on subclassing, we do not
      // emit ACC_FINAL.
      (
        (((sym.rawflags & Flags.FINAL) != 0) || sym.isTopLevelModuleClass)
          && !sym.enclClass.isInterface
          && !sym.isClassConstructor
          && !sym.isMutable // lazy vals and vars both
        )
    }

    def isJavaEntryPoint: Boolean = {
      def fail(msg: String, pos: Position = sym.pos) = {
        reporter.warning(sym.pos,
          sym.name +
            s" has a main method with parameter type Array[String], but ${sym.fullName('.')} will not be a runnable program.\n  Reason: $msg"
          // TODO: make this next claim true, if possible
          //   by generating valid main methods as static in module classes
          //   not sure what the jvm allows here
          // + "  You can still run the program by calling it as " + sym.javaSimpleName + " instead."
        )
        false
      }
      def failNoForwarder(msg: String) = {
        fail(s"$msg, which means no static forwarder can be generated.\n")
      }
      val possibles = if (sym.hasModuleFlag) (sym.tpe nonPrivateMember nme.main).alternatives else Nil
      val hasApproximate = possibles exists { m =>
        m.info match {
          case MethodType(p :: Nil, _) => p.tpe.typeSymbol == definitions.ArrayClass
          case _                       => false
        }
      }
      // At this point it's a module with a main-looking method, so either succeed or warn that it isn't.
      hasApproximate && {
        // Before erasure so we can identify generic mains.
        enteringErasure {
          val companion     = sym.linkedClassOfClass

          if (definitions.hasJavaMainMethod(companion))
            failNoForwarder("companion contains its own main method")
          else if (companion.tpe.member(nme.main) != NoSymbol)
          // this is only because forwarders aren't smart enough yet
            failNoForwarder("companion contains its own main method (implementation restriction: no main is allowed, regardless of signature)")
          else if (companion.isTrait)
            failNoForwarder("companion is a trait")
          // Now either succeeed, or issue some additional warnings for things which look like
          // attempts to be java main methods.
          else (possibles exists definitions.isJavaMainMethod) || {
            possibles exists { m =>
              m.info match {
                case PolyType(_, _) =>
                  fail("main methods cannot be generic.")
                case MethodType(params, res) =>
                  if (res.typeSymbol :: params exists (_.isAbstractType))
                    fail("main methods cannot refer to type parameters or abstract types.", m.pos)
                  else
                    definitions.isJavaMainMethod(m) || fail("main method must have exact signature (Array[String])Unit", m.pos)
                case tp =>
                  fail(s"don't know what this is: $tp", m.pos)
              }
            }
          }
        }
      }
    }
  }

  def log(msg: => String): Unit = global synchronized { global.log(msg) }

  def sourceFileFor(cu: CompilationUnit): String = cu.source.toString

  def noForwarders: Boolean = settings.noForwarders

  var pickledBytes = 0

  /*  Returns a ScalaSignature annotation if it must be added to this class, none otherwise.
 *  This annotation must be added to the class' annotations list when generating them.
 *
 *  Depending on whether the returned option is defined, it adds to `jclass` one of:
 *    (a) the ScalaSig marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is present in this class); or
 *    (b) the Scala marker attribute
 *        (indicating that a scala-signature-annotation aka pickle is to be found in another file).
 *
 *
 *  @param jclassName The class file that is being readied.
 *  @param sym    The symbol for which the signature has been entered in the symData map.
 *                This is different than the symbol
 *                that is being generated in the case of a mirror class.
 *  @return       An option that is:
 *                - defined and contains an AnnotationInfo of the ScalaSignature type,
 *                  instantiated with the pickle signature for sym.
 *                - empty if the jclass/sym pair must not contain a pickle.
 *
 *  must-single-thread
 */
  def getAnnotPickle(jclassName: String, sym: Symbol): Option[AnnotationInfo] = {
    currentRun.symData get sym match {
      case Some(pickle) if !nme.isModuleName(newTermName(jclassName)) =>
        val scalaAnnot = {
          val sigBytes = ScalaSigBytes(pickle.bytes.take(pickle.writeIndex))
          AnnotationInfo(sigBytes.sigAnnot, Nil, (nme.bytes, sigBytes) :: Nil)
        }
        pickledBytes += pickle.writeIndex
        currentRun.symData -= sym
        currentRun.symData -= sym.companionSymbol
        Some(scalaAnnot)
      case _ =>
        None
    }
  }

  object ScalaCTypeHelper extends TypeHelper {

    var t: Type = _

    def members: List[Symbol] = t.members.toList
    def <:<(other: Type): Boolean = t <:< other
    def isFinalType: Boolean = t.isFinalType
    def member(string: Name): Symbol = t.member(string)
    def paramTypes: List[Type] = t.paramTypes
    def underlying: Type = t.underlying
    def memberInfo(s: Symbol): Type = t.memberInfo(s)
    def decls: List[Symbol] = t.decls.toList
    def typeSymbol: Symbol = t.typeSymbol
    def =:=(other: Type): Boolean = t =:= other
    def membersBasedOnFlags(excludedFlags: Flags, requiredFlags: Flags): List[Symbol] = t.membersBasedOnFlags(excludedFlags, requiredFlags).toList
    def resultType: Type = t.resultType

    def summaryString: String = t.summaryString

    def parents: List[Type] = t.parents

    def params: List[Symbol] = t.params

    /**
     * This method returns the BType for a type reference, for example a parameter type.
     *
     * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
     *
     * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
     * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
     * classes.
     */
    def toTypeKind(ctx: BCodeHelpers)(storage: ctx.BCInnerClassGen): ctx.bTypes.BType = {
      import ctx.bTypes._
      import coreBTypes._
      /**
       * Primitive types are represented as TypeRefs to the class symbol of, for example, scala.Int.
       * The `primitiveTypeMap` maps those class symbols to the corresponding PrimitiveBType.
       */
      def primitiveOrClassToBType(sym: Symbol): BType = {
        assert(sym.isClass, sym)
        assert(sym != ArrayClass || isCompilingArray, sym)
        primitiveTypeMap.getOrElse(sym, storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ctx.int.Symbol]))
      }

      /**
       * When compiling Array.scala, the type parameter T is not erased and shows up in method
       * signatures, e.g. `def apply(i: Int): T`. A TyperRef to T is replaced by ObjectReference.
       */
      def nonClassTypeRefToBType(sym: Symbol): ClassBType = {
        assert(sym.isType && isCompilingArray, sym)
        ObjectReference
      }

      t.dealiasWiden match {
        case TypeRef(_, ArrayClass, List(arg))  => ArrayBType(arg.toTypeKind(ctx)(storage))  // Array type such as Array[Int] (kept by erasure)
        case TypeRef(_, sym, _) if !sym.isClass => nonClassTypeRefToBType(sym)  // See comment on nonClassTypeRefToBType
        case TypeRef(_, sym, _)                 => primitiveOrClassToBType(sym) // Common reference to a type such as scala.Int or java.lang.String
        case ClassInfoType(_, _, sym)           => primitiveOrClassToBType(sym) // We get here, for example, for genLoadModule, which invokes toTypeKind(moduleClassSymbol.info)

        /* AnnotatedType should (probably) be eliminated by erasure. However we know it happens for
         * meta-annotated annotations (@(ann @getter) val x = 0), so we don't emit a warning.
         * The type in the AnnotationInfo is an AnnotatedTpe. Tested in jvm/annotations.scala.
         */
        case a @ AnnotatedType(_, t) =>
          debuglog(s"typeKind of annotated type $a")
          t.toTypeKind(ctx)(storage)

        /* ExistentialType should (probably) be eliminated by erasure. We know they get here for
         * classOf constants:
         *   class C[T]
         *   class T { final val k = classOf[C[_]] }
         */
        case e @ ExistentialType(_, t) =>
          debuglog(s"typeKind of existential type $e")
          t.toTypeKind(ctx)(storage)

        /* The cases below should probably never occur. They are kept for now to avoid introducing
         * new compiler crashes, but we added a warning. The compiler / library bootstrap and the
         * test suite don't produce any warning.
         */

        case tp =>
          currentUnit.warning(tp.typeSymbol.pos,
            s"an unexpected type representation reached the compiler backend while compiling $currentUnit: $tp. " +
              "If possible, please file a bug on issues.scala-lang.org.")

          tp match {
            case ThisType(ArrayClass)               => ObjectReference // was introduced in 9b17332f11 to fix SI-999, but this code is not reached in its test, or any other test
            case ThisType(sym)                      => storage.getClassBTypeAndRegisterInnerClass(sym.asInstanceOf[ctx.int.Symbol])
            case SingleType(_, sym)                 => primitiveOrClassToBType(sym)
            case ConstantType(_)                    => t.underlying.toTypeKind(ctx)(storage)
            case RefinedType(parents, _)            => parents.map(_.toTypeKind(ctx)(storage).asClassBType).reduceLeft((a, b) => a.jvmWiseLUB(b))
          }
      }
    }
  }

  object ScalacNameHelper extends NameHelper {
    var n: Name = _
    import global.AnyNameOps
    def offset: Int = n.start

    def toTypeName: Name = n.toTypeName

    def isTypeName: Boolean = n.isTypeName

    def toTermName: Name = n.toTermName

    def dropModule: Name = AnyNameOps(n).dropModule

    def len: Int = n.length

    def isTermName: Boolean = n.isTermName

    def startsWith(s: String): Boolean = n.startsWith(s)
  }

  object ScalacTreeHelper extends TreeHelper{
    var t: Tree = _
    def symbol: Symbol = t.symbol

    def pos: Position = t.pos

    def isEmpty: Boolean = t.isEmpty

    def tpe: Type = t.tpe

    def exists(pred: (Tree) => Boolean): Boolean = t.exists(pred)
  }

  object ScalacAnnotationHelper extends AnnotationHelper {
    var t: Annotation = _

    def atp: Type = t.atp
    def assocs: List[(Name, Object)] = t.assocs
    def symbol: Symbol = t.symbol
    def args: List[Tree] = t.args
  }

  object ScalacConstantHelper extends ConstantHelper {
    var c: Constant = _
    def tag: ConstantTag = c.tag

    def booleanValue: Boolean = c.booleanValue
    def longValue: Long = c.longValue
    def byteValue: Byte = c.byteValue
    def stringValue: String = c.stringValue
    def symbolValue: Symbol = c.symbolValue
    def floatValue: Float = c.floatValue
    def value: Any = c.value
    def typeValue: Type = c.typeValue
    def shortValue: Short = c.shortValue
    def intValue: Int = c.intValue
    def doubleValue: Double = c.doubleValue
    def charValue: Char = c.charValue
  }

  object ScalacPositionHelper extends PositionHelper {
    var p: Position = _

    def isDefined: Boolean = p.isDefined
    def line: Int = p.line
    def finalPosition: Position = p.finalPosition
  }

  def isQualifierSafeToElide(qual: Tree): Boolean = treeInfo isQualifierSafeToElide qual
  def desugarIdent(i: Ident): Option[Select] = None // dotty specific

  def isBox(sym: Symbol): Boolean = currentRun.runDefinitions.isBox(sym)

  def isUnbox(sym: Symbol): Boolean = currentRun.runDefinitions.isUnbox(sym)

  /*
     * For arg a LiteralAnnotArg(constt) with const.tag in {ClazzTag, EnumTag}
     * as well as for arg a NestedAnnotArg
     *   must-single-thread
     * Otherwise it's safe to call from multiple threads.
     */
  private def emitArgument(av:   asm.AnnotationVisitor,
                           name: String,
                           arg:  ClassfileAnnotArg, bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    (arg: @unchecked) match {

      case LiteralAnnotArg(const) =>
        if (const.isNonUnitAnyVal) { av.visit(name, const.value) }
        else {
          const.tag match {
            case StringTag  =>
              assert(const.value != null, const) // TODO this invariant isn't documented in `case class Constant`
              av.visit(name, const.stringValue)  // `stringValue` special-cases null, but that execution path isn't exercised for a const with StringTag
            case ClazzTag   => av.visit(name, const.typeValue.toTypeKind(bcodeStore)(innerClasesStore).toASMType)
            case EnumTag =>
              val edesc  = innerClasesStore.typeDescriptor(const.tpe.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the enumeration class.
              val evalue = const.symbolValue.name.toString // value the actual enumeration value.
              av.visitEnum(name, edesc, evalue)
          }
        }

      case sb @ ScalaSigBytes(bytes) =>
        // see http://www.scala-lang.org/sid/10 (Storage of pickled Scala signatures in class files)
        // also JVMS Sec. 4.7.16.1 The element_value structure and JVMS Sec. 4.4.7 The CONSTANT_Utf8_info Structure.
        if (sb.fitsInOneString) {
          av.visit(name, BCodeAsmCommon.strEncode(sb))
        } else {
          val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
          for(arg <- BCodeAsmCommon.arrEncode(sb)) { arrAnnotV.visit(name, arg) }
          arrAnnotV.visitEnd()
        }          // for the lazy val in ScalaSigBytes to be GC'ed, the invoker of emitAnnotations() should hold the ScalaSigBytes in a method-local var that doesn't escape.

      case ArrayAnnotArg(args) =>
        val arrAnnotV: asm.AnnotationVisitor = av.visitArray(name)
        for(arg <- args) { emitArgument(arrAnnotV, null, arg, bcodeStore)(innerClasesStore) }
        arrAnnotV.visitEnd()

      case NestedAnnotArg(annInfo) =>
        val AnnotationInfo(typ, args, assocs) = annInfo
        assert(args.isEmpty, args)
        val desc = innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]) // the class descriptor of the nested annotation class
      val nestedVisitor = av.visitAnnotation(name, desc)
        emitAssocs(nestedVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * In general,
   *   must-single-thread
   * but not  necessarily always.
   */
  private def emitAssocs(av: asm.AnnotationVisitor, assocs: List[(Name, ClassfileAnnotArg)], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for ((name, value) <- assocs) {
      emitArgument(av, name.toString(), value, bcodeStore)(innerClasesStore)
    }
    av.visitEnd()
  }

  /*
   * must-single-thread
   */
  override def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = cw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  override def emitAnnotations(mw: asm.MethodVisitor, annotations: List[AnnotationInfo], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = mw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  override def emitAnnotations(fw: asm.FieldVisitor, annotations: List[AnnotationInfo], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    for(annot <- annotations; if shouldEmitAnnotation(annot)) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val av = fw.visitAnnotation(innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(av, assocs, bcodeStore)(innerClasesStore)
    }
  }

  /*
   * must-single-thread
   */
  override def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[AnnotationInfo]], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen) {
    val annotationss = pannotss map (_ filter shouldEmitAnnotation)
    if (annotationss forall (_.isEmpty)) return
    for ((annots, idx) <- annotationss.zipWithIndex;
         annot <- annots) {
      val AnnotationInfo(typ, args, assocs) = annot
      assert(args.isEmpty, args)
      val pannVisitor: asm.AnnotationVisitor = jmethod.visitParameterAnnotation(idx, innerClasesStore.typeDescriptor(typ.asInstanceOf[bcodeStore.int.Type]), isRuntimeVisible(annot))
      emitAssocs(pannVisitor, assocs, bcodeStore)(innerClasesStore)
    }
  }

  def getGenericSignature(sym: Symbol, owner: Symbol): String = genASM.getGenericSignature(sym, owner)

  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String = genASM.staticForwarderGenericSignature(sym, moduleClass)

  def getLabelDefOwners(t: Tree): Map[Tree, List[LabelDef]] = {

    class LabelDefsFinder extends Traverser {
      val result = collection.mutable.Map.empty[Tree, List[LabelDef]]
      var acc: List[LabelDef] = Nil

      /*
       * can-multi-thread
       */
      override def traverse(tree: Tree) {
        val saved = acc
        acc = Nil
        super.traverse(tree)
        // acc contains all LabelDefs found under (but not at) `tree`
        tree match {
          case lblDf: LabelDef => acc ::= lblDf
          case _               => ()
        }
        if (acc.isEmpty) {
          acc = saved
        } else {
          result += (tree -> acc)
          acc = acc ::: saved
        }
      }
    }
    val ldf = new LabelDefsFinder()
    ldf.traverse(t)
    ldf.result.toMap
  }

  implicit val TypeDefTag: ClassTag[TypeDef] = global.TypeDefTag
  implicit val ApplyTag: ClassTag[Apply] = global.ApplyTag
  implicit val SelectTag: ClassTag[Select] = global.SelectTag
  implicit val TypeApplyTag: ClassTag[TypeApply] = global.TypeApplyTag
  implicit val ClassDefTag: ClassTag[ClassDef] = global.ClassDefTag
  implicit val TryTag: ClassTag[Try] = global.TryTag
  implicit val AssignTag: ClassTag[Assign] = global.AssignTag
  implicit val IdentTag: ClassTag[Ident] = global.IdentTag
  implicit val IfTag: ClassTag[If] = global.IfTag
  implicit val LabelDefTag: ClassTag[LabelDef] = global.LabelDefTag
  implicit val ValDefTag: ClassTag[ValDef] = global.ValDefTag
  implicit val ThrowTag: ClassTag[Throw] = global.ThrowTag
  implicit val ReturnTag: ClassTag[Return] = global.ReturnTag
  implicit val LiteralTag: ClassTag[Literal] = global.LiteralTag
  implicit val BlockTag: ClassTag[Block] = global.BlockTag
  implicit val TypedTag: ClassTag[Typed] = global.TypedTag
  implicit val ArrayValueTag: ClassTag[ArrayValue] = ClassTag[ArrayValue](classOf[ArrayValue])
  implicit val MatchTag: ClassTag[Match] = global.MatchTag
  implicit val CaseDefTag: ClassTag[CaseDef] = global.CaseDefTag
  implicit val ThisTag: ClassTag[This] = global.ThisTag
  implicit val AlternativeTag: ClassTag[Alternative] = global.AlternativeTag
  implicit val DefDefTag: ClassTag[DefDef] = global.DefDefTag
  implicit val ModuleDefTag: ClassTag[ModuleDef] = global.ModuleDefTag
  implicit val NameTag: ClassTag[Name] = global.NameTag
  implicit val TemplateTag: ClassTag[Template] = global.TemplateTag
  implicit val BindTag: ClassTag[Bind] = global.BindTag
  implicit val NewTag: ClassTag[New] = global.NewTag
  implicit val ApplyDynamicTag: ClassTag[ApplyDynamic] = ClassTag[ApplyDynamic](classOf[ApplyDynamic])
  implicit val SuperTag: ClassTag[Super] = global.SuperTag
  implicit val ConstantClassTag: ClassTag[Constant] = global.ConstantTag
  implicit val ClosureTag: ClassTag[Closure] = ClassTag[Closure](classOf[Closure])
}
