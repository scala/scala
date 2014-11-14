package scala.tools.nsc.backend.jvm

import scala.collection.generic.Clearable
import scala.reflect.ClassTag
import scala.reflect.io.AbstractFile
import scala.tools.nsc.Global
import scala.language.implicitConversions
import scala.tools.asm


/* Interface to abstract over frontend inside backend.
 * Intended to be implemented by both scalac and dotc
 */
trait BackendInterface extends BackendInterfaceDefinitions{
  type Flags      = Long

  type Symbol     >: Null <: AnyRef
  type Type       >: Null <: AnyRef
  type Annotation >: Null <: AnyRef
  type Tree       >: Null <: AnyRef
  type TypeDef    >: Null <: Tree
  type Apply      >: Null <: Tree
  type TypeApply  >: Null <: Tree
  type ClassDef   >: Null <: Tree
  type Try        >: Null <: Tree
  type Assign     >: Null <: Tree
  type Ident      >: Null <: Tree
  type If         >: Null <: Tree
  type LabelDef   >: Null <: Tree
  type ValDef     >: Null <: Tree
  type Throw      >: Null <: Tree
  type Return     >: Null <: Tree
  type Literal >: Null <: Tree
  type Block >: Null <: Tree
  type Typed >: Null <: Tree
  type ArrayValue >: Null <: Tree
  type Match >: Null <: Tree
  type This >: Null <: Tree
  type CaseDef >: Null <: Tree
  type Alternative >: Null <: Tree
  type DefDef >: Null <: Tree
  type ModuleDef >: Null <: Tree
  type Template >: Null <: Tree
  type Name
  type Position
  type CompilationUnit <: AnyRef



  type Constant
  type ConstantTag = Int

  val UnitTag: ConstantTag
  val IntTag: ConstantTag
  val FloatTag: ConstantTag
  val NullTag: ConstantTag
  val BooleanTag: ConstantTag
  val ByteTag: ConstantTag
  val ShortTag: ConstantTag
  val CharTag: ConstantTag
  val DoubleTag: ConstantTag
  val LongTag: ConstantTag
  val StringTag: ConstantTag
  val ClazzTag: ConstantTag
  val EnumTag: ConstantTag

  val primitives: Primitives


  val nme_This: Name
  val nme_EMPTY_PACKAGE_NAME: Name
  val nme_CONSTRUCTOR: Name
  val nme_WILDCARD: Name
  val nme_THIS: Name
  val nme_PACKAGE: Name
  val nme_EQEQ_LOCAL_VAR: Name

  /* methods used to box&unbox primitives ?and value classes? */
  def boxMethods: Map[Symbol, Symbol] // (class, method)
  def unboxMethods: Map[Symbol, Symbol]

  /*
   * Collects all LabelDef nodes enclosed (directly or not) by each node.
   *
   * In other words, this prepares a map giving
   * all labelDefs (the entry-value) having a Tree node (the entry-key) as ancestor.
   * The entry-value for a LabelDef entry-key always contains the entry-key.
   */
  def getLabelDefOwners(t: Tree): Map[Tree, List[LabelDef]]

  /*
   * Implementation of those methods is very specific to how annotations are represented
   *  representations for Dotc and Scalac are to different to abstract over them
   */
  def emitAnnotations(cw: asm.ClassVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitAnnotations(mw: asm.MethodVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitAnnotations(fw: asm.FieldVisitor, annotations: List[Annotation], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit
  def emitParamAnnotations(jmethod: asm.MethodVisitor, pannotss: List[List[Annotation]], bcodeStore: BCodeHelpers)(innerClasesStore: bcodeStore.BCInnerClassGen): Unit

  /* means of getting class symbols from compiler */
  def requiredClass[T: ClassTag]: Symbol
  def requiredModule[T: ClassTag]: Symbol
  def getRequiredClass(fullname: String): Symbol
  def getClassIfDefined(fullname: String): Symbol

  def isQualifierSafeToElide(qual: Tree): Boolean

  /* various configuration options used by backend */
  def emitAsmp: Option[String]
  def dumpClasses: Option[String]
  def mainClass: Option[String]
  def noForwarders: Boolean
  def debuglevel: Int
  def settings_debug: Boolean
  def targetPlatform: String
  def sourceFileFor(cu: CompilationUnit): String
  def setMainClass(name: String): Unit
  def informProgress(msg: String): Unit

  /* backend actually uses free names to generate stuff. This should NOT mangled */
  def newTermName(prefix: String): Name

  def getGenericSignature(sym: Symbol, owner:Symbol): String
  def getStaticForwarderGenericSignature(sym: Symbol, moduleClass: Symbol): String

  def isBox(sym: Symbol): Boolean
  def isUnbox(sym: Symbol): Boolean
  def isMaybeBoxed(sym: Symbol): Boolean

  /** Whether an annotation should be emitted as a Java annotation
    * .initialize: if 'annot' is read from pickle, atp might be un-initialized
    */
  def shouldEmitAnnotation(annot: Annotation): Boolean

  def isRuntimeVisible(annot: Annotation): Boolean

  def getSingleOutput: Option[AbstractFile]

  implicit def symHelper(sym: Symbol): SymbolHelper
  implicit def typeHelper(tp: Type): TypeHelper
  implicit def nameHelper(n: Name): NameHelper
  implicit def annotHelper(a: Annotation): AnnotationHelper
  implicit def treeHelper(a: Tree): TreeHelper

  implicit def constantHelper(a: Constant): ConstantHelper
  implicit def positionHelper(a: Position): PositionHelper


  val Assign: AssignDeconstructor
  val Select: SelectDeconstructor
  val Apply: ApplyDeconstructor
  val If: IfDeconstructor
  val ValDef: ValDefDeconstructor
  val Throw: ThrowDeconstructor
  val New: NewDeconstructor
  val ApplyDynamic: ApplyDynamicDeconstructor
  val This: ThisDeconstructor
  val Ident: IdentDeconstructor
  val Try: TryDeconstructor
  val Return: ReturnDeconstructor
  val LabelDef: LabelDeconstructor
  val Literal: LiteralDeconstructor
  val Typed: TypedDeconstrutor
  val Super: SuperDeconstructor
  val ArrayValue: ArrayValueDeconstructor
  val Match: MatchDeconstructor
  val Block: BlockDeconstructor
  val TypeApply: TypeApplyDeconstructor
  val CaseDef: CaseDeconstructor
  val Alternative: AlternativeDeconstructor
  val Constant: ConstantDeconstructor
  val ThrownException: ThrownException
  val DefDef: DefDefDeconstructor
  val ModuleDef: ModuleDefDeconstructor
  val Template: TemplateDeconstructor
  val Bind: BindDeconstructor
  val ClassDef: ClassDefDeconstructor

  trait ClassDefDeconstructor{
    def unapply(a: Tree): Option[(Any /*Modifiers*/, Name, List[TypeDef], Template)]
  }

  trait BindDeconstructor{
    def unapply(a: Tree): Option[(Name, Tree)]
  }

  trait TemplateDeconstructor {
    def unapply(a: Tree): Option[(List[Tree], ValDef, List[Tree])]
  }

  trait ModuleDefDeconstructor{
    def unapply(a: Tree): Option[(/*Modifiers*/ Any, Name, Tree)]
  }

  trait DefDefDeconstructor{
    def unapply(a: Tree): Option[(/*Modifiers*/ Any, Name, List[TypeDef], List[List[ValDef]], Tree, Tree)]
  }

  trait ThrownException{
    def unapply(a: Annotation): Option[Symbol]
  }

  trait ConstantDeconstructor{
    def unapply(a: Any): Option[Any]
  }

  trait BlockDeconstructor{
    def unapply(a: Tree): Option[(List[Tree], Tree)]
  }

  trait CaseDeconstructor{
    def unapply(a: Tree): Option[(Tree, Tree, Tree)]
  }

  trait MatchDeconstructor{
    def unapply(a: Tree): Option[(Tree, List[Tree])]
  }

  trait LiteralDeconstructor{
    def unapply(a: Tree): Option[Constant]
  }

  trait AssignDeconstructor{
    def unapply(a: Tree): Option[(Tree, Tree)]
  }

  trait SelectDeconstructor{
    def unapply(s: Tree): Option[(Tree, Name)]
  }

  trait ApplyDeconstructor {
    def unapply(s: Tree): Option[(Tree, List[Tree])]
  }

  trait IfDeconstructor{
    def unapply(s: Tree): Option[(Tree, Tree, Tree)]
  }

  trait ValDefDeconstructor{
    def unapply(s: Tree): Option[(Nothing /* Modifiers*/, Name, Tree, Tree)]
  }

  trait ThrowDeconstructor{
    def unapply(s: Tree): Option[Tree]
  }

  trait NewDeconstructor{
    def unapply(s: Tree): Option[Type]
  }

  trait ApplyDynamicDeconstructor{
    def unapply(s: Tree): Option[(Tree, List[Tree])]
  }

  trait ThisDeconstructor{
    def unapply(s: Tree): Option[Name]
    def apply(s: Symbol): This
  }

  trait IdentDeconstructor{
    def unapply(s: Tree): Option[Name]
  }

  trait TryDeconstructor{
    def unapply(s: Tree): Option[(Tree, List[Tree], Tree)]
  }

  trait ReturnDeconstructor {
    def unapply(s: Tree): Option[Tree]
  }

  trait LabelDeconstructor {
    def unapply(s: Tree): Option[(Name, List[Ident], Tree)]
  }

  trait TypedDeconstrutor {
    def unapply(s: Tree): Option[(Tree, Tree)]
  }

  trait SuperDeconstructor{
    def unapply(s: Tree): Option[(Tree, Name)]
  }

  trait ArrayValueDeconstructor {
    def unapply(s: Tree): Option[(Tree, List[Tree])]
  }

  trait TypeApplyDeconstructor {
    def unapply(s: Tree): Option[(Tree, List[Tree])]
  }

  trait AlternativeDeconstructor {
    def unapply(S: Tree): Option[List[Tree]]
  }

  trait PositionHelper {
    def isDefined: Boolean
    def finalPosition: Position
    def line: Int
  }

  trait ConstantHelper {
    def tag: ConstantTag
    def longValue: Long
    def doubleValue: Double
    def charValue: Char
    def stringValue: String
    def byteValue: Byte
    def booleanValue: Boolean
    def shortValue: Short
    def intValue: Int
    def value: Any
    def floatValue: Float
    def typeValue: Type
    def symbolValue: Symbol
  }

  trait TreeHelper{
    def symbol: Symbol
    def tpe: Type
    def isEmpty: Boolean
    def pos: Position
    def exists(pred: Tree => Boolean): Boolean
  }
  
  trait SymbolHelper {
    def info: Type
    def isClass: Boolean
    def isType: Boolean
    def isAnonymousClass: Boolean
    def isConstructor: Boolean
    def primaryConstructor: Symbol
    def tpe: Type // todo whats the differentce between tpe and info?
    def thisType: Type

    def freshLocal(name: String, pos: Position, flags: Flags): Symbol

    def isAnonymousFunction: Boolean
    def originalOwner: Symbol
    def parentSymbols: List[Symbol]
    def isMethod: Boolean
    def isPublic: Boolean
    def isSynthetic: Boolean
    def enclClass: Symbol
    def isPackageClass: Boolean
    def isModuleClass: Boolean
    def isModule: Boolean
    def isStrictFP: Boolean
    def isLabel: Boolean
    def hasPackageFlag: Boolean
    def fullName(sep: Char): String
    def fullName: String
    def simpleName: String
    def javaSimpleName: Name
    def javaBinaryName: Name
    def javaClassName: String
    def isImplClass: Boolean
    def superClass: Symbol
    def isInterface: Boolean
    def nestedClasses: List[Symbol]
    def linkedClass: Symbol
    def companionClass: Symbol
    def companionModule: Symbol
    def companionSymbol: Symbol
    def annotations: List[Annotation]
    def moduleClass: Symbol
    def shouldEmitForwarders: Boolean
    def hasGetter: Boolean
    def isGetter: Boolean
    def isSetter: Boolean
    def isGetClass: Boolean
    def getter(clz: Symbol): Symbol
    def setter(clz: Symbol): Symbol
    def memberClasses: List[Symbol]
    def linkedClassOfClass: Symbol

    def companionModuleMembers: List[Symbol]
    def isJavaDefined: Boolean
    def isDeferred: Boolean
    def isPrivate: Boolean
    def isFinal: Boolean
    def isStaticMember: Boolean
    def isBottomClass: Boolean
    def isNonBottomSubClass(sym: Symbol): Boolean
    def isBridge: Boolean
    def isArtifact: Boolean
    def hasEnumFlag: Boolean
    def hasAccessBoundary: Boolean
    def isVarargsMethod: Boolean
    def isDeprecated: Boolean
    def isSynchronized: Boolean
    def name: Name
    def rawname: Name // todo ????
    def owner: Symbol
    def rawowner: Symbol // todo ???
    def enclosingClassSym: Symbol
    def hasAnnotation(sym: Symbol): Boolean
    def isMutable: Boolean
    def hasAbstractFlag: Boolean
    def hasModuleFlag: Boolean
    def isClassConstructor: Boolean
    def moduleSuffix: String
    def outputDirectory: AbstractFile
    def isJavaEntryPoint: Boolean
    def serialVUID: Option[Long]
    def pos: Position

    def fieldSymbols: List[Symbol]
    def throwsAnnotations: List[Symbol]

    def methodSymbols: List[Symbol]
    /**
     * All interfaces implemented by a class, except for those inherited through the superclass.
     *
     */
    def superInterfaces: List[Symbol]

    /**
     * True for module classes of package level objects. The backend will generate a mirror class for
     * such objects.
     */
    def isTopLevelModuleClass: Boolean

    /**
     * True for module classes of modules that are top-level or owned only by objects. Module classes
     * for such objects will get a MODULE$ flag and a corresponding static initializer.
     */
    def isStaticModuleClass: Boolean

    def isStaticConstructor: Boolean

    /**
     * This is basically a re-implementation of sym.isStaticOwner, but using the originalOwner chain.
     *
     * The problem is that we are interested in a source-level property. Various phases changed the
     * symbol's properties in the meantime, mostly lambdalift modified (destructively) the owner.
     * Therefore, `sym.isStatic` is not what we want. For example, in
     *   object T { def f { object U } }
     * the owner of U is T, so UModuleClass.isStatic is true. Phase travel does not help here.
     */
    def isOriginallyStaticOwner: Boolean


    def addRemoteRemoteExceptionAnnotation: Unit
  }

  trait TypeHelper {
    def <:<(other: Type): Boolean
    def =:=(other: Type): Boolean
    def paramTypes: List[Type]
    def params: List[Symbol]
    def resultType: Type
    def memberInfo(s: Symbol): Type
    def membersBasedOnFlags(excludedFlags: Flags, requiredFlags: Flags): List[Symbol]
    def members: List[Symbol]
    def decls: List[Symbol]
    def underlying: Type
    def parents: List[Type]
    def summaryString: String
    def typeSymbol: Symbol
    def member(string: Name): Symbol
    def members(string: Name): List[Symbol]
    /**
     * This method returns the BType for a type reference, for example a parameter type.
     *
     * If the result is a ClassBType for a nested class, it is added to the innerClassBufferASM.
     *
     * If `t` references a class, toTypeKind ensures that the class is not an implementation class.
     * See also comment on getClassBTypeAndRegisterInnerClass, which is invoked for implementation
     * classes.
     */
    def toTypeKind(ctx: BCodeHelpers)(storage: ctx.BCInnerClassGen): ctx.bTypes.BType

    def isFinalType: Boolean
  }

  trait Primitives {
    def getPrimitive(methodSym: Symbol, reciever: Type): Int
    def isPrimitive(sym: Symbol): Boolean
    def getPrimitive(sym: Symbol): Int
  }

  trait NameHelper {
    def offset: Int
    def index = offset
    def start = offset
    def len: Int
    def length = len
    def toTypeName: Name
    def isTypeName: Boolean
    def isTermName: Boolean
    def toTermName: Name
    def dropModule: Name
    def startsWith(s: String): Boolean
  }

  trait AnnotationHelper{
    def atp: Type
    def symbol: Symbol
    def args: List[Tree]
    def assocs: List[(Name, /* ClassfileAnnotArg*/ Object)]
  }

  def debuglog(msg: => String): Unit
  def log(msg: => String): Unit
  def error(pos: Position, msg: String): Unit // reporter.error
  def warning(pos: Position, msg: String): Unit // reporter.warning
  def abort(msg: String): Nothing

  val ExcludedForwarderFlags: Flags
  val Flag_METHOD: Flags
  val Flag_SYNTHETIC: Flags

  /** Some useful equality helpers. */
  def isNull(t: Tree): Boolean
  def isLiteral(t: Tree): Boolean
  def isNonNullExpr(t: Tree): Boolean

  /** If l or r is constant null, returns the other ; otherwise null */
  def ifOneIsNull(l: Tree, r: Tree): Tree

  def isCompilingPrimitive: Boolean
  def isCompilingArray: Boolean

  trait Caches {
    def recordCache[T <: Clearable](cache: T): T
    def newWeakMap[K, V](): collection.mutable.WeakHashMap[K, V]
    def newMap[K, V](): collection.mutable.HashMap[K, V]
    def newSet[K](): collection.mutable.Set[K]
    def newWeakSet[K <: AnyRef](): reflect.internal.util.WeakHashSet[K]
    def newAnyRefMap[K <: AnyRef, V](): collection.mutable.AnyRefMap[K, V]
  }

  def perRunCaches: Caches

  def MODULE_INSTANCE_FIELD: String
  def internalNameString(offset: Int, length: Int): String
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
  def getAnnotPickle(jclassName: String, sym: Symbol): Option[Annotation]
}

trait BackendInterfaceDefinitions { self: BackendInterface =>
  val nme_valueOf: Name

  /* magic instances */
  val NoPosition: Position
  val NoSymbol: Symbol
  val EmptyTree: Tree
  val NothingClass: Symbol
  val NullClass: Symbol
  val ObjectClass: Symbol
  val Object_isInstanceOf: Symbol
  val Object_asInstanceOf: Symbol
  val Object_equals: Symbol


  val UnitClass: Symbol = requiredClass[scala.Unit]
  val BooleanClass: Symbol = requiredClass[scala.Boolean]
  val CharClass: Symbol  = requiredClass[scala.Char]
  val ShortClass: Symbol = requiredClass[scala.Short]
  val ClassClass: Symbol = requiredClass[java.lang.Class[_]]
  val ByteClass: Symbol  = requiredClass[scala.Byte]
  val IntClass: Symbol = requiredClass[scala.Int]
  val LongClass: Symbol = requiredClass[scala.Long]
  val FloatClass: Symbol = requiredClass[scala.Float]
  val DoubleClass: Symbol  = requiredClass[scala.Double]

  // Class symbols used in backend.
  // Vals becouse they are to frequent in scala programs so that they are already loaded by backend

  lazy val RemoteAttr: Symbol = requiredClass[scala.remote]
  lazy val BeanInfoAttr: Symbol = requiredClass[scala.beans.BeanInfo]
  lazy val NativeAttr: Symbol = requiredClass[scala.native]
  lazy val TransientAttr = requiredClass[scala.transient]
  lazy val VolatileAttr = requiredClass[scala.volatile]
  val ScalaSignatureATTRName: String = "ScalaSig"

  val BoxedBooleanClass: Symbol = requiredClass[java.lang.Boolean]
  val BoxedByteClass: Symbol = requiredClass[java.lang.Byte]
  val BoxedShortClass: Symbol = requiredClass[java.lang.Short]
  val BoxedCharacterClass: Symbol = requiredClass[java.lang.Character]
  val BoxedIntClass: Symbol  = requiredClass[java.lang.Integer]
  val BoxedLongClass: Symbol = requiredClass[java.lang.Long]
  val BoxedFloatClass: Symbol = requiredClass[java.lang.Float]
  val BoxedDoubleClass: Symbol = requiredClass[java.lang.Double]
  val StringClass: Symbol = requiredClass[java.lang.String]
  val StringBuilderClass: Symbol = requiredClass[java.lang.StringBuilder]
  val ThrowableClass: Symbol = requiredClass[java.lang.Throwable]
  val JavaCloneableClass: Symbol = requiredClass[java.lang.Cloneable]
  val NullPointerExceptionClass: Symbol  = requiredClass[java.lang.NullPointerException]
  val JavaSerializableClass: Symbol = requiredClass[java.io.Serializable]
  val SerializableClass: Symbol = requiredClass[scala.Serializable]
  val ClassCastExceptionClass: Symbol = requiredClass[java.lang.ClassCastException]
  val ArrayClass: Symbol = requiredClass[scala.Array[_]]
  val ClassfileAnnotationClass: Symbol = requiredClass[scala.annotation.ClassfileAnnotation]
  val BoxedNumberClass: Symbol = requiredClass[java.lang.Number]
  val RemoteExceptionClass: Symbol = requiredClass[java.rmi.RemoteException]
  val ThrowsClass: Symbol = requiredClass[scala.throws[_]]

  // Module symbols used in backend
  val StringModule: Symbol = StringClass.linkedClassOfClass
  val ScalaRunTimeModule: Symbol = requiredModule[scala.runtime.ScalaRunTime.type]

  // methods used in backend

  val Array_clone: Symbol
  val hashMethodSym: Symbol
  val externalEqualsNumNum: Symbol
  val externalEqualsNumChar: Symbol
  val externalEqualsNumObject: Symbol
  val externalEquals: Symbol

  val MaxFunctionArity: Int
  val FunctionClass: Array[Symbol]
  val AbstractFunctionClass: Array[Symbol]
  val PartialFunctionClass: Symbol
  val AbstractPartialFunctionClass: Symbol

  /* The Object => String overload. */
  lazy val String_valueOf: Symbol = {
    StringModule.info.members(nme_valueOf).filter(sym => sym.info.paramTypes match {
      case List(pt) => pt.typeSymbol == ObjectClass
      case _        => false
    }).head
  }
}