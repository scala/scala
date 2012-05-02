package scala.reflect

import scala.reflect.api.AbsTreeGen
import scala.reflect.api.Attachment
import scala.reflect.api.Modifier
import scala.reflect.api.Universe

// todo. make Dummy objects not equal to themselves
class DummyMirror(cl: ClassLoader) extends api.Mirror {
  // Members declared in scala.reflect.api.AnnotationInfos
  implicit def classfileAnnotArgTag: scala.reflect.ClassTag[ClassfileAnnotArg] = notSupported()
  type AnnotationInfo = DummyAnnotationInfo.type
  object DummyAnnotationInfo
  val AnnotationInfo: AnnotationInfoExtractor = DummyAnnotationInfoExtractor
  object DummyAnnotationInfoExtractor extends AnnotationInfoExtractor {
    def apply(atp: Type, args: List[Tree], assocs: List[(Name, ClassfileAnnotArg)]): AnnotationInfo = DummyAnnotationInfo
    def unapply(info: AnnotationInfo): Option[(Type, List[Tree], List[(Name, ClassfileAnnotArg)])] = notSupported()
  }
  type ClassfileAnnotArg = AnyRef
  type LiteralAnnotArg = DummyLiteralAnnotArg.type
  object DummyLiteralAnnotArg
  val LiteralAnnotArg: LiteralAnnotArgExtractor = DummyLiteralAnnotArgExtractor
  type ArrayAnnotArg = DummyArrayAnnotArg.type
  object DummyArrayAnnotArg
  val ArrayAnnotArg: ArrayAnnotArgExtractor = DummyArrayAnnotArgExtractor
  type NestedAnnotArg = DummyNestedAnnotArg.type
  object DummyNestedAnnotArg
  val NestedAnnotArg: NestedAnnotArgExtractor = DummyNestedAnnotArgExtractor
  object DummyLiteralAnnotArgExtractor extends LiteralAnnotArgExtractor {
    def apply(const: Constant): LiteralAnnotArg = DummyLiteralAnnotArg
    def unapply(arg: LiteralAnnotArg): Option[Constant] = notSupported()
  }
  object DummyArrayAnnotArgExtractor extends ArrayAnnotArgExtractor {
    def apply(const: Array[ClassfileAnnotArg]): ArrayAnnotArg = DummyArrayAnnotArg
    def unapply(arg: ArrayAnnotArg): Option[Array[ClassfileAnnotArg]] = notSupported()
  }
  object DummyNestedAnnotArgExtractor extends NestedAnnotArgExtractor {
    def apply(anninfo: AnnotationInfo): NestedAnnotArg = DummyNestedAnnotArg
    def unapply(arg: NestedAnnotArg): Option[AnnotationInfo] = notSupported()
  }

  // Members declared in scala.reflect.api.Constants
  type Constant = DummyConstant.type
  object DummyConstant extends AbsConstant {
    val value: Any = notSupported()
    def tpe: Type = notSupported()
    def isNaN: Boolean = notSupported()
    def booleanValue: Boolean = notSupported()
    def byteValue: Byte = notSupported()
    def shortValue: Short = notSupported()
    def charValue: Char = notSupported()
    def intValue: Int = notSupported()
    def longValue: Long = notSupported()
    def floatValue: Float = notSupported()
    def doubleValue: Double = notSupported()
    def stringValue: String = notSupported()
    def typeValue: Type = notSupported()
    def symbolValue: Symbol = notSupported()
    def convertTo(pt: Type): Constant = notSupported()
  }
  val Constant: ConstantExtractor = DummyConstantExtractor
  object DummyConstantExtractor extends ConstantExtractor {
    def apply(const: Any): Constant = DummyConstant
    def unapply(arg: Constant): Option[Any] = notSupported()
  }

  // Members declared in scala.reflect.api.FreeVars
  type FreeTerm = DummyFreeTerm.type
  val DummyFreeTerm = DummySymbol
  val FreeTerm: FreeTermExtractor = DummyFreeTermExtractor
  object DummyFreeTermExtractor extends FreeTermExtractor {
    def unapply(freeTerm: FreeTerm): Option[(TermName, Type, Any, String)] = notSupported()
  }
  type FreeType = DummyFreeType.type
  val DummyFreeType = DummySymbol
  val FreeType: FreeTypeExtractor = DummyFreeTypeExtractor
  object DummyFreeTypeExtractor extends FreeTypeExtractor {
    def unapply(freeType: FreeType): Option[(TypeName, Type, String)] = notSupported()
  }
  def freeTerms(tree: Tree): List[FreeTerm] = notSupported()
  def freeTypes(tree: Tree): List[FreeType] = notSupported()
  def substituteFreeTypes(tpe: Type,subs: Map[FreeType,Type]): Type = notSupported()
  def substituteFreeTypes(tree: Tree,subs: Map[FreeType,Type]): Tree = notSupported()

  // Members declared in scala.reflect.api.Importers
  def mkImporter(from0: scala.reflect.api.Universe): Importer{val from: from0.type} = notSupported()

  // Members declared in scala.reflect.api.Mirror
  def classLoader: ClassLoader = cl
  def classLoader_=(x$1: ClassLoader): Unit = notSupported()
  def classToSymbol(clazz: Class[_]): Symbol = notSupported()
  def classToType(clazz: Class[_]): Type = notSupported()
  def companionInstance(clazz: Symbol): AnyRef = notSupported()
  def getValueOfField(receiver: AnyRef,field: Symbol): Any = notSupported()
  def invoke(receiver: AnyRef,meth: Symbol)(args: Any*): Any = notSupported()
  def setValueOfField(receiver: AnyRef,field: Symbol,value: Any): Unit = notSupported()
  def symbolForName(name: String): Symbol = notSupported()
  def symbolOfInstance(instance: Any): Symbol = notSupported()
  def symbolToClass(sym: Symbol): Class[_] = notSupported()
  def typeOfInstance(instance: Any): Type = notSupported()
  def typeToClass(tpe: Type): Class[_] = notSupported()

  // Members declared in scala.reflect.api.Names
  type Name = DummyName.type
  type TypeName = DummyName.type
  type TermName = DummyName.type
  object DummyName extends AbsName {
    def isTermName: Boolean = notSupported()
    def isTypeName: Boolean = notSupported()
    def toTermName: TermName = notSupported()
    def toTypeName: TypeName = notSupported()
    def decoded: String = notSupported()
    def encoded: String = notSupported()
    def decodedName: Name = notSupported()
    def encodedName: Name = notSupported()
  }
  def newTermName(s: String): TermName = notSupported()
  def newTypeName(s: String): TypeName = notSupported()

  // Members declared in scala.reflect.api.Positions
  type Position = DummyPosition.type
  object DummyPosition extends api.Position {
    def pos: Position = notSupported()
    def withPos(newPos: scala.reflect.api.Position): Attachment = notSupported()
    def payload: Any = notSupported()
    def withPayload(newPayload: Any): Attachment = notSupported()
    def fileInfo: java.io.File = notSupported()
    def fileContent: Array[Char] = notSupported()
    def isDefined: Boolean = notSupported()
    def isTransparent: Boolean = notSupported()
    def isRange: Boolean = notSupported()
    def isOpaqueRange: Boolean = notSupported()
    def makeTransparent: Position = notSupported()
    def start: Int = notSupported()
    def startOrPoint: Int = notSupported()
    def point: Int = notSupported()
    def pointOrElse(default: Int): Int = notSupported()
    def end: Int = notSupported()
    def endOrPoint: Int = notSupported()
    def withStart(off: Int): Position = notSupported()
    def withEnd(off: Int): Position = notSupported()
    def withPoint(off: Int): Position = notSupported()
    def union(pos: scala.reflect.api.Position): Position = notSupported()
    def focusStart: Position = notSupported()
    def focus: Position = notSupported()
    def focusEnd: Position = notSupported()
    def includes(pos: scala.reflect.api.Position): Boolean = notSupported()
    def properlyIncludes(pos: scala.reflect.api.Position): Boolean = notSupported()
    def precedes(pos: scala.reflect.api.Position): Boolean = notSupported()
    def properlyPrecedes(pos: scala.reflect.api.Position): Boolean = notSupported()
    def overlaps(pos: scala.reflect.api.Position): Boolean = notSupported()
    def sameRange(pos: scala.reflect.api.Position): Boolean = notSupported()
    def line: Int = notSupported()
    def column: Int = notSupported()
    def toSingleLine: Position = notSupported()
    def lineContent: String = notSupported()
    def show: String = notSupported()
  }
  val NoPosition: Position = DummyPosition
  def atPos[T <: Tree](pos: Position)(tree: T): T = tree
  def ensureNonOverlapping(tree: Tree,others: List[Tree]): Unit = notSupported()
  def wrappingPos(trees: List[Tree]): Position = notSupported()
  def wrappingPos(default: Position,trees: List[Tree]): Position = notSupported()

  // Members declared in scala.reflect.api.FrontEnds
  def mkConsoleFrontEnd(minSeverity: Int): FrontEnd = notSupported()

  // Members declared in scala.reflect.api.Scopes
  type Scope = DummyScope.type
  object DummyScope extends Iterable[Symbol] {
    def iterator: Iterator[Symbol] = notSupported()
  }
  def newScope: Scope = DummyScope
  def newScopeWith(elems: Symbol*): Scope = DummyScope
  def newNestedScope(outer: Scope): Scope = DummyScope

  // Members declared in scala.reflect.api.StandardDefinitions
  val AnyRefTpe: Type = DummyType
  val AnyTpe: Type = DummyType
  val AnyValTpe: Type = DummyType
  val BooleanTpe: Type = DummyType
  val ByteTpe: Type = DummyType
  val CharTpe: Type = DummyType
  val DoubleTpe: Type = DummyType
  val FloatTpe: Type = DummyType
  val IntTpe: Type = DummyType
  val LongTpe: Type = DummyType
  val NothingTpe: Type = DummyType
  val NullTpe: Type = DummyType
  val ObjectTpe: Type = DummyType
  val ShortTpe: Type = DummyType
  val StringTpe: Type = DummyType
  val UnitTpe: Type = DummyType
  val definitions: AbsDefinitions = DummyDefinitions
  object DummyDefinitions extends AbsDefinitions {
    def ByNameParamClass = DummySymbol
    def JavaRepeatedParamClass = DummySymbol
    def RepeatedParamClass = DummySymbol
    def AnyClass = DummyClassSymbol
    def AnyRefClass = DummyTypeSymbol
    def AnyValClass = DummyClassSymbol
    def ArrayClass = DummyClassSymbol
    def ArrayModule = DummySymbol
    def ArrayModule_overloadedApply = DummySymbol
    def Array_apply = DummySymbol
    def Array_clone = DummySymbol
    def Array_length = DummySymbol
    def Array_update = DummySymbol
    def BooleanClass = DummyClassSymbol
    def ByteClass = DummyClassSymbol
    def CharClass = DummyClassSymbol
    def ClassClass = DummyClassSymbol
    def ClassTagClass = DummyClassSymbol
    def ClassTagModule = DummySymbol
    def ConcreteTypeTagClass = DummyClassSymbol
    def ConcreteTypeTagModule = DummySymbol
    def ConsClass = DummySymbol
    def DoubleClass = DummyClassSymbol
    def EmptyPackage = DummyPackageSymbol
    def EmptyPackageClass = DummySymbol
    def FloatClass = DummyClassSymbol
    def FunctionClass: Array[Symbol] = Array()
    def IntClass = DummyClassSymbol
    def IterableClass = DummySymbol
    def IteratorClass = DummySymbol
    def IteratorModule = DummySymbol
    def Iterator_apply = DummySymbol
    def JavaLangPackage = DummyPackageSymbol
    def JavaLangPackageClass = DummySymbol
    def ListClass = DummyClassSymbol
    def ListModule = DummyModuleSymbol
    def List_apply = DummySymbol
    def LongClass = DummyClassSymbol
    def NilModule = DummySymbol
    def NoneModule = DummySymbol
    def NothingClass = DummyClassSymbol
    def NullClass = DummyClassSymbol
    def ObjectClass = DummyClassSymbol
    def OptionClass = DummySymbol
    def PredefModule = DummyModuleSymbol
    def ProductClass: Array[Symbol] = Array()
    def RootClass = DummyClassSymbol
    def RootPackage = DummyPackageSymbol
    def ScalaPackage = DummyPackageSymbol
    def ScalaPackageClass = DummySymbol
    def ScalaPrimitiveValueClasses = Nil
    def SeqClass = DummySymbol
    def SeqModule = DummySymbol
    def ShortClass = DummyClassSymbol
    def SomeClass = DummySymbol
    def SomeModule = DummySymbol
    def StringBuilderClass = DummySymbol
    def StringClass = DummyClassSymbol
    def SymbolClass = DummySymbol
    def TraversableClass = DummySymbol
    def TupleClass: Array[Symbol] = Array()
    def TypeTagClass = DummyClassSymbol
    def TypeTagModule = DummySymbol
    def UnitClass = DummyClassSymbol
    def isNumericValueClass(sym: Symbol): Boolean = notSupported()
    def isPrimitiveValueClass(sym: Symbol): Boolean = notSupported()
    def vmClassType(arg: Type): Type = DummyType
    def vmSignature(sym: Symbol,info: Type): String = notSupported()
  }

  // Members declared in scala.reflect.api.StandardNames
  val nme: AbsTermNames = DummyAbsTermNames
  val tpnme: AbsTypeNames = DummyAbsTypeNames
  object DummyAbsTermNames extends AbsTermNames {
    type NameType = TermName
    val EMPTY: NameType = DummyName
    val ANON_FUN_NAME: NameType = DummyName
    val ANON_CLASS_NAME: NameType = DummyName
    val EMPTY_PACKAGE_NAME: NameType = DummyName
    val IMPORT: NameType = DummyName
    val MODULE_VAR_SUFFIX: NameType = DummyName
    val ROOT: NameType = DummyName
    val PACKAGE: NameType = DummyName
    val SPECIALIZED_SUFFIX: NameType = DummyName
    val ERROR: NameType = DummyName
    val NO_NAME: NameType = DummyName
    val WILDCARD: NameType = DummyName
    def flattenedName(segments: Name*): NameType = notSupported()
    val EXPAND_SEPARATOR_STRING: String = ""
    val ANYNAME: TermName = DummyName
    val CONSTRUCTOR: TermName = DummyName
    val FAKE_LOCAL_THIS: TermName = DummyName
    val INITIALIZER: TermName = DummyName
    val LAZY_LOCAL: TermName = DummyName
    val LOCAL_SUFFIX_STRING: String = ""
    val MIRROR_PREFIX: TermName = DummyName
    val MIRROR_SHORT: TermName = DummyName
    val MIRROR_FREE_PREFIX: TermName = DummyName
    val MIRROR_FREE_THIS_SUFFIX: TermName = DummyName
    val MIRROR_FREE_VALUE_SUFFIX: TermName = DummyName
    val MIRROR_SYMDEF_PREFIX: TermName = DummyName
    val MIXIN_CONSTRUCTOR: TermName = DummyName
    val MODULE_INSTANCE_FIELD: TermName = DummyName
    val OUTER: TermName = DummyName
    val OUTER_LOCAL: TermName = DummyName
    val OUTER_SYNTH: TermName = DummyName
    val SELECTOR_DUMMY: TermName = DummyName
    val SELF: TermName = DummyName
    val SPECIALIZED_INSTANCE: TermName = DummyName
    val STAR: TermName = DummyName
    val THIS: TermName = DummyName
    val BITMAP_NORMAL: TermName = DummyName
    val BITMAP_TRANSIENT: TermName = DummyName
    val BITMAP_CHECKINIT: TermName = DummyName
    val BITMAP_CHECKINIT_TRANSIENT: TermName = DummyName
    val INTERPRETER_IMPORT_WRAPPER: String = ""
    val INTERPRETER_LINE_PREFIX: String = ""
    val INTERPRETER_VAR_PREFIX: String = ""
    val INTERPRETER_WRAPPER_SUFFIX: String = ""
    val ROOTPKG: TermName = DummyName
    val ADD: TermName = DummyName
    val AND: TermName = DummyName
    val ASR: TermName = DummyName
    val DIV: TermName = DummyName
    val EQ: TermName = DummyName
    val EQL: TermName = DummyName
    val GE: TermName = DummyName
    val GT: TermName = DummyName
    val HASHHASH: TermName = DummyName
    val LE: TermName = DummyName
    val LSL: TermName = DummyName
    val LSR: TermName = DummyName
    val LT: TermName = DummyName
    val MINUS: TermName = DummyName
    val MOD: TermName = DummyName
    val MUL: TermName = DummyName
    val NE: TermName = DummyName
    val OR: TermName = DummyName
    val PLUS : TermName = DummyName
    val SUB: TermName = DummyName
    val XOR: TermName = DummyName
    val ZAND: TermName = DummyName
    val ZOR: TermName = DummyName
    val UNARY_~ : TermName = DummyName
    val UNARY_+ : TermName = DummyName
    val UNARY_- : TermName = DummyName
    val UNARY_! : TermName = DummyName
    val ??? : TermName = DummyName
    val MODULE_SUFFIX_NAME: TermName = DummyName
    val NAME_JOIN_NAME: TermName = DummyName
    val IMPL_CLASS_SUFFIX: String = ""
    val LOCALDUMMY_PREFIX: String = ""
    val PROTECTED_PREFIX: String = ""
    val PROTECTED_SET_PREFIX: String = ""
    val SINGLETON_SUFFIX: String = ""
    val SUPER_PREFIX_STRING: String = ""
    val TRAIT_SETTER_SEPARATOR_STRING: String = ""
    val SETTER_SUFFIX: TermName = DummyName
    def isConstructorName(name: Name): Boolean = notSupported()
    def isExceptionResultName(name: Name): Boolean = notSupported()
    def isImplClassName(name: Name): Boolean = notSupported()
    def isLocalDummyName(name: Name): Boolean = notSupported()
    def isLocalName(name: Name): Boolean = notSupported()
    def isLoopHeaderLabel(name: Name): Boolean = notSupported()
    def isProtectedAccessorName(name: Name): Boolean = notSupported()
    def isSuperAccessorName(name: Name): Boolean = notSupported()
    def isReplWrapperName(name: Name): Boolean = notSupported()
    def isSetterName(name: Name): Boolean = notSupported()
    def isTraitSetterName(name: Name): Boolean = notSupported()
    def isSingletonName(name: Name): Boolean = notSupported()
    def isModuleName(name: Name): Boolean = notSupported()
    def isOpAssignmentName(name: Name): Boolean = notSupported()
    def segments(name: String, assumeTerm: Boolean): List[Name] = notSupported()
    def originalName(name: Name): Name = notSupported()
    def stripModuleSuffix(name: Name): Name = notSupported()
    def unspecializedName(name: Name): Name = notSupported()
    def splitSpecializedName(name: Name): (Name, String, String) = notSupported()
    def dropLocalSuffix(name: Name): Name = notSupported()
    def expandedName(name: TermName, base: Symbol, separator: String = EXPAND_SEPARATOR_STRING): TermName = notSupported()
    def expandedSetterName(name: TermName, base: Symbol): TermName = notSupported()
    def protName(name: Name): TermName = notSupported()
    def protSetterName(name: Name): TermName = notSupported()
    def getterName(name: TermName): TermName = notSupported()
    def getterToLocal(name: TermName): TermName = notSupported()
    def getterToSetter(name: TermName): TermName = notSupported()
    def localToGetter(name: TermName): TermName = notSupported()
    def setterToGetter(name: TermName): TermName = notSupported()
    def defaultGetterName(name: Name, pos: Int): TermName = notSupported()
    def defaultGetterToMethod(name: Name): TermName = notSupported()
    def localDummyName(clazz: Symbol): TermName = notSupported()
    def superName(name: Name): TermName = notSupported()
  }
  object DummyAbsTypeNames extends AbsTypeNames {
    type NameType = TypeName
    val EMPTY: NameType = DummyName
    val ANON_FUN_NAME: NameType = DummyName
    val ANON_CLASS_NAME: NameType = DummyName
    val EMPTY_PACKAGE_NAME: NameType = DummyName
    val IMPORT: NameType = DummyName
    val MODULE_VAR_SUFFIX: NameType = DummyName
    val ROOT: NameType = DummyName
    val PACKAGE: NameType = DummyName
    val SPECIALIZED_SUFFIX: NameType = DummyName
    val ERROR: NameType = DummyName
    val NO_NAME: NameType = DummyName
    val WILDCARD: NameType = DummyName
    def flattenedName(segments: Name*): NameType = notSupported()
    val REFINE_CLASS_NAME: TypeName = DummyName
    val BYNAME_PARAM_CLASS_NAME: TypeName = DummyName
    val EQUALS_PATTERN_NAME: TypeName = DummyName
    val JAVA_REPEATED_PARAM_CLASS_NAME: TypeName = DummyName
    val LOCAL_CHILD: TypeName = DummyName
    val REPEATED_PARAM_CLASS_NAME: TypeName = DummyName
    val WILDCARD_STAR: TypeName = DummyName

    def dropSingletonName(name: Name): TypeName = notSupported()
    def singletonName(name: Name): TypeName = notSupported()
    def implClassName(name: Name): TypeName = notSupported()
    def interfaceName(implname: Name): TypeName = notSupported()
  }

  // Members declared in scala.reflect.api.Symbols
  val NoSymbol = DummySymbol
  type Symbol = DummySymbolApi
  object DummySymbol extends DummySymbolApi
  type TypeSymbol = DummyTypeSymbolApi
  object DummyTypeSymbol extends DummyTypeSymbolApi
  type TermSymbol = DummyTermSymbolApi
  object DummyTermSymbol extends DummyTermSymbolApi
  type MethodSymbol = DummyMethodSymbolApi
  object DummyMethodSymbol extends DummyMethodSymbolApi
  type ModuleSymbol = DummyModuleSymbolApi
  object DummyModuleSymbol extends DummyModuleSymbolApi
  type PackageSymbol = DummyPackageSymbolApi
  object DummyPackageSymbol extends DummyPackageSymbolApi
  type ClassSymbol = DummyClassSymbolApi
  object DummyClassSymbol extends DummyClassSymbolApi
  trait DummySymbolApi extends AbsSymbol {
    this: Symbol =>

    def pos: Position = notSupported()
    def modifiers: Set[Modifier] = notSupported()
    def hasModifier(mod: Modifier): Boolean = notSupported()
    def annotations: List[AnnotationInfo] = notSupported()
    def hasAnnotation(sym: Symbol): Boolean = notSupported()
    def owner: Symbol = notSupported()
    def name: Name = notSupported()
    def fullName: String = notSupported()
    def id: Int = notSupported()
    def orElse(alt: => Symbol): Symbol = notSupported()
    def filter(cond: Symbol => Boolean): Symbol = notSupported()
    def suchThat(cond: Symbol => Boolean): Symbol = notSupported()
    def privateWithin: Symbol = notSupported()
    def companionSymbol: Symbol = notSupported()
    def moduleClass: Symbol = notSupported()
    def enclosingTopLevelClass: Symbol = notSupported()
    def enclosingClass: Symbol = notSupported()
    def enclosingMethod: Symbol = notSupported()
    def enclosingPackageClass: Symbol = notSupported()
    def isTerm         : Boolean = notSupported()
    def isPackage      : Boolean = notSupported()
    def isMethod       : Boolean = notSupported()
    def isOverloaded   : Boolean = notSupported()
    def isFreeTerm     : Boolean = notSupported()
    def isType         : Boolean = notSupported()
    def isClass        : Boolean = notSupported()
    def isPackageClass  : Boolean = notSupported()
    def isPrimitiveValueClass: Boolean = notSupported()
    def isDerivedValueClass: Boolean = notSupported()
    def isAliasType    : Boolean = notSupported()
    def isAbstractType : Boolean = notSupported()
    def isSkolem       : Boolean = notSupported()
    def isExistential  : Boolean = notSupported()
    def isFreeType     : Boolean = notSupported()
    def isContravariant : Boolean = notSupported()
    def isCovariant     : Boolean = notSupported()
    def isErroneous : Boolean = notSupported()
    def typeSignature: Type = notSupported()
    def typeSignatureIn(site: Type): Type = notSupported()
    def asType: Type = notSupported()
    def asTypeIn(site: Type): Type = notSupported()
    def asTypeConstructor: Type = notSupported()
    def thisPrefix: Type = notSupported()
    def selfType: Type = notSupported()
    def alternatives: List[Symbol] = notSupported()
    def resolveOverloaded(pre: Type = NoPrefix, targs: Seq[Type] = List(), actuals: Seq[Type]): Symbol = notSupported()
    def newNestedSymbol(name: Name, pos: Position, flags: Long, isClass: Boolean): Symbol = notSupported()
    def setInternalFlags(flags: Long): this.type = notSupported()
    def setTypeSignature(tpe: Type): this.type = notSupported()
    def setAnnotations(annots: AnnotationInfo*): this.type = notSupported()
    def kind: String = notSupported()
  }
  trait DummyTypeSymbolApi extends DummySymbolApi with TypeSymbolApi {
    this: TypeSymbol =>
  }
  trait DummyTermSymbolApi extends DummySymbolApi with TermSymbolApi {
    this: TermSymbol =>
  }
  trait DummyMethodSymbolApi extends DummyTermSymbolApi with MethodSymbolApi {
    this: MethodSymbol =>
  }
  trait DummyModuleSymbolApi extends DummyTermSymbolApi with ModuleSymbolApi {
    this: ModuleSymbol =>
  }
  trait DummyPackageSymbolApi extends DummyModuleSymbolApi with PackageSymbolApi {
    this: PackageSymbol =>
  }
  trait DummyClassSymbolApi extends DummyTypeSymbolApi with ClassSymbolApi {
    this: ClassSymbol =>
  }

  // Members declared in scala.reflect.api.ToolBoxes
  def mkToolBox(frontEnd: FrontEnd, options: String): AbsToolBox = notSupported()

  // Members declared in scala.reflect.api.TreeBuildUtil
  // type TreeGen = DummyTreeGen.type // [Eugene] cannot compile if uncomment this
  val gen: TreeGen{val global: DummyMirror.this.type} = DummyTreeGen.asInstanceOf[TreeGen{val global: DummyMirror.this.type}]
  def modifiersFromInternalFlags(flags: Long,privateWithin: Name,annotations: List[Tree]): Modifiers = DummyModifiers
  def newFreeExistential(name: String,info: Type,value: => Any,flags: Long,origin: String) = DummySymbol
  def newFreeTerm(name: String,info: Type,value: => Any,flags: Long,origin: String) = DummySymbol
  def newFreeType(name: String,info: Type,value: => Any,flags: Long,origin: String) = DummySymbol
  def selectOverloadedMethod(owner: Symbol,name: String,index: Int) = DummySymbol
  def selectOverloadedMethodIfDefined(owner: Symbol,name: String,index: Int) = DummySymbol
  def selectTerm(owner: Symbol,name: String) = DummySymbol
  def selectTermIfDefined(owner: Symbol,name: String) = DummySymbol
  def selectType(owner: Symbol,name: String) = DummySymbol
  def selectTypeIfDefined(owner: Symbol,name: String) = DummySymbol
  def staticClass(fullName: String) = DummySymbol
  def staticClassIfDefined(fullName: String) = DummySymbol
  def staticModule(fullName: String) = DummySymbol
  def staticModuleIfDefined(fullName: String) = DummySymbol
  def thisModuleType(fullName: String): Type = DummyType
  object DummyTreeGen extends AbsTreeGen {
    val global: Universe = DummyMirror.this
    type TreeGenTree = global.Tree
    type TreeGenType = global.Type
    type TreeGenSymbol = global.Symbol
    type TreeGenName = global.Name
    def mkAttributedQualifier(tpe: TreeGenType): TreeGenTree = notSupported()
    def mkAttributedQualifier(tpe: TreeGenType, termSym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkAttributedRef(pre: TreeGenType, sym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkAttributedRef(sym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkAttributedThis(sym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkAttributedIdent(sym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkAttributedSelect(qual: TreeGenTree, sym: TreeGenSymbol): TreeGenTree = notSupported()
    def mkMethodCall(target: TreeGenTree,targs: List[TreeGenType],args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(receiver: TreeGenTree,method: TreeGenSymbol,targs: List[TreeGenType],args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(receiver: TreeGenSymbol,methodName: TreeGenName,args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(target: TreeGenTree,args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(method: TreeGenSymbol,args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(method: TreeGenSymbol,targs: List[TreeGenType],args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkMethodCall(receiver: TreeGenSymbol,methodName: TreeGenName,targs: List[TreeGenType],args: List[TreeGenTree]): TreeGenTree = notSupported()
    def mkNullaryCall(method: TreeGenSymbol,targs: List[TreeGenType]): TreeGenTree = notSupported()
  }

  // Members declared in scala.reflect.api.TreePrinters
  def newTreePrinter(out: java.io.PrintWriter): TreePrinter = notSupported()

  // Members declared in scala.reflect.api.Trees
  def Apply(sym: Symbol,args: Tree*): Tree = Apply(EmptyTree, Nil)
  def Bind(sym: Symbol,body: Tree): Bind = Bind(DummyName, EmptyTree)
  def Block(stats: Tree*): Block = Block()
  def CaseDef(pat: Tree,body: Tree): CaseDef = CaseDef(EmptyTree, EmptyTree, EmptyTree)
  def ClassDef(sym: Symbol,impl: Template): ClassDef = ClassDef(DummyModifiers, DummyName, Nil, Template(Nil, emptyValDef, Nil))
  def DefDef(sym: Symbol,rhs: List[List[Symbol]] => Tree): DefDef = DefDef(DummyModifiers, DummyName, Nil, Nil, EmptyTree, EmptyTree)
  def DefDef(sym: Symbol,rhs: Tree): DefDef = DefDef(DummyModifiers, DummyName, Nil, Nil, EmptyTree, EmptyTree)
  def DefDef(sym: Symbol,mods: Modifiers,rhs: Tree): DefDef = DefDef(DummyModifiers, DummyName, Nil, Nil, EmptyTree, EmptyTree)
  def DefDef(sym: Symbol,vparamss: List[List[ValDef]],rhs: Tree): DefDef = DefDef(DummyModifiers, DummyName, Nil, Nil, EmptyTree, EmptyTree)
  def DefDef(sym: Symbol,mods: Modifiers,vparamss: List[List[ValDef]],rhs: Tree): DefDef = DefDef(DummyModifiers, DummyName, Nil, Nil, EmptyTree, EmptyTree)
  def Ident(sym: Symbol): Ident = Ident(DummyName)
  def Ident(name: String): Ident = Ident(DummyName)
  def LabelDef(sym: Symbol,params: List[Symbol],rhs: Tree): LabelDef = LabelDef(DummyName, Nil, EmptyTree)
  type Modifiers = DummyModifiers.type
  val NoMods: Modifiers = DummyModifiers
  object DummyModifiers extends AbsModifiers {
    def modifiers: Set[Modifier] = notSupported()
    def hasModifier(mod: Modifier): Boolean = notSupported()
    def privateWithin: Name = notSupported()
    def annotations: List[Tree] = notSupported()
    def mapAnnotations(f: List[Tree] => List[Tree]): Modifiers = notSupported()
  }
  def Modifiers(mods: Set[scala.reflect.api.Modifier],privateWithin: Name,annotations: List[Tree]): Modifiers = DummyModifiers
  def ModuleDef(sym: Symbol,impl: Template): ModuleDef = ModuleDef(DummyModifiers, DummyName, Template(Nil, emptyValDef, Nil))
  def New(sym: Symbol,args: Tree*): Tree = New(EmptyTree)
  def New(tpe: Type,args: Tree*): Tree = New(EmptyTree)
  def New(tpt: Tree,argss: List[List[Tree]]): Tree = New(EmptyTree)
  def Select(qualifier: Tree,sym: Symbol): Select = Select(EmptyTree, DummyName)
  def Select(qualifier: Tree,name: String): Select = Select(EmptyTree, DummyName)
  def Super(sym: Symbol,mix: TypeName): Tree = Super(EmptyTree, DummyName)
  def This(sym: Symbol): Tree = This(DummyName)
  def Throw(tpe: Type,args: Tree*): Throw = Throw(EmptyTree)
  def Try(body: Tree,cases: (Tree, Tree)*): Try = Try(EmptyTree)
  def TypeDef(sym: Symbol): TypeDef = TypeDef(DummyModifiers, DummyName, Nil, EmptyTree)
  def TypeDef(sym: Symbol,rhs: Tree): TypeDef = TypeDef(DummyModifiers, DummyName, Nil, EmptyTree)
  def ValDef(sym: Symbol): ValDef = ValDef(DummyModifiers, DummyName, EmptyTree, EmptyTree)
  def ValDef(sym: Symbol,rhs: Tree): ValDef = ValDef(DummyModifiers, DummyName, EmptyTree, EmptyTree)
  protected def duplicateTree(tree: Tree): Tree = notSupported()
  object emptyValDef extends ValDef(DummyModifiers, DummyName, EmptyTree, EmptyTree) { override def isEmpty = true }
  type TreeCopier = DummyTreeCopier.type
  def newStrictTreeCopier: TreeCopier = DummyTreeCopier
  def newLazyTreeCopier: TreeCopier = DummyTreeCopier
  object DummyTreeCopier extends TreeCopierOps {
    def ClassDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], impl: Template): ClassDef = notSupported()
    def PackageDef(tree: Tree, pid: RefTree, stats: List[Tree]): PackageDef = notSupported()
    def ModuleDef(tree: Tree, mods: Modifiers, name: Name, impl: Template): ModuleDef = notSupported()
    def ValDef(tree: Tree, mods: Modifiers, name: Name, tpt: Tree, rhs: Tree): ValDef = notSupported()
    def DefDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: Tree): DefDef = notSupported()
    def TypeDef(tree: Tree, mods: Modifiers, name: Name, tparams: List[TypeDef], rhs: Tree): TypeDef = notSupported()
    def LabelDef(tree: Tree, name: Name, params: List[Ident], rhs: Tree): LabelDef = notSupported()
    def Import(tree: Tree, expr: Tree, selectors: List[ImportSelector]): Import = notSupported()
    def Template(tree: Tree, parents: List[Tree], self: ValDef, body: List[Tree]): Template = notSupported()
    def Block(tree: Tree, stats: List[Tree], expr: Tree): Block = notSupported()
    def CaseDef(tree: Tree, pat: Tree, guard: Tree, body: Tree): CaseDef = notSupported()
    def Alternative(tree: Tree, trees: List[Tree]): Alternative = notSupported()
    def Star(tree: Tree, elem: Tree): Star = notSupported()
    def Bind(tree: Tree, name: Name, body: Tree): Bind = notSupported()
    def UnApply(tree: Tree, fun: Tree, args: List[Tree]): UnApply = notSupported()
    def ArrayValue(tree: Tree, elemtpt: Tree, trees: List[Tree]): ArrayValue = notSupported()
    def Function(tree: Tree, vparams: List[ValDef], body: Tree): Function = notSupported()
    def Assign(tree: Tree, lhs: Tree, rhs: Tree): Assign = notSupported()
    def AssignOrNamedArg(tree: Tree, lhs: Tree, rhs: Tree): AssignOrNamedArg = notSupported()
    def If(tree: Tree, cond: Tree, thenp: Tree, elsep: Tree): If = notSupported()
    def Match(tree: Tree, selector: Tree, cases: List[CaseDef]): Match = notSupported()
    def Return(tree: Tree, expr: Tree): Return = notSupported()
    def Try(tree: Tree, block: Tree, catches: List[CaseDef], finalizer: Tree): Try = notSupported()
    def Throw(tree: Tree, expr: Tree): Throw = notSupported()
    def New(tree: Tree, tpt: Tree): New = notSupported()
    def Typed(tree: Tree, expr: Tree, tpt: Tree): Typed = notSupported()
    def TypeApply(tree: Tree, fun: Tree, args: List[Tree]): TypeApply = notSupported()
    def Apply(tree: Tree, fun: Tree, args: List[Tree]): Apply = notSupported()
    def ApplyDynamic(tree: Tree, qual: Tree, args: List[Tree]): ApplyDynamic = notSupported()
    def Super(tree: Tree, qual: Tree, mix: TypeName): Super = notSupported()
    def This(tree: Tree, qual: Name): This = notSupported()
    def Select(tree: Tree, qualifier: Tree, selector: Name): Select = notSupported()
    def Ident(tree: Tree, name: Name): Ident = notSupported()
    def ReferenceToBoxed(tree: Tree, idt: Ident): ReferenceToBoxed = notSupported()
    def Literal(tree: Tree, value: Constant): Literal = notSupported()
    def TypeTree(tree: Tree): TypeTree = notSupported()
    def Annotated(tree: Tree, annot: Tree, arg: Tree): Annotated = notSupported()
    def SingletonTypeTree(tree: Tree, ref: Tree): SingletonTypeTree = notSupported()
    def SelectFromTypeTree(tree: Tree, qualifier: Tree, selector: Name): SelectFromTypeTree = notSupported()
    def CompoundTypeTree(tree: Tree, templ: Template): CompoundTypeTree = notSupported()
    def AppliedTypeTree(tree: Tree, tpt: Tree, args: List[Tree]): AppliedTypeTree = notSupported()
    def TypeBoundsTree(tree: Tree, lo: Tree, hi: Tree): TypeBoundsTree = notSupported()
    def ExistentialTypeTree(tree: Tree, tpt: Tree, whereClauses: List[Tree]): ExistentialTypeTree = notSupported()
  }

  // Members declared in scala.reflect.api.Types
  type Type = DummyType.type
  type SingletonType = DummyType.type
  type CompoundType = DummyType.type
  type AnnotatedType = DummyType.type
  val AnnotatedType: AnnotatedTypeExtractor = DummyAnnotatedTypeExtractor
  type BoundedWildcardType = DummyType.type
  val BoundedWildcardType: BoundedWildcardTypeExtractor = DummyBoundedWildcardTypeExtractor
  type ClassInfoType = DummyType.type
  val ClassInfoType: ClassInfoTypeExtractor = DummyClassInfoTypeExtractor
  type ConstantType = DummyType.type
  val ConstantType: ConstantTypeExtractor = DummyConstantTypeExtractor
  type ExistentialType = DummyType.type
  val ExistentialType: ExistentialTypeExtractor = DummyExistentialTypeExtractor
  type MethodType = DummyType.type
  val MethodType: MethodTypeExtractor = DummyMethodTypeExtractor
  val NoPrefix: Type = DummyType
  val NoType: Type = DummyType
  type NullaryMethodType = DummyType.type
  val NullaryMethodType: NullaryMethodTypeExtractor = DummyNullaryMethodTypeExtractor
  type PolyType = DummyType.type
  val PolyType: PolyTypeExtractor = DummyPolyTypeExtractor
  type RefinedType = DummyType.type
  val RefinedType: RefinedTypeExtractor = DummyRefinedTypeExtractor
  type SingleType = DummyType.type
  val SingleType: SingleTypeExtractor = DummySingleTypeExtractor
  type SuperType = DummyType.type
  val SuperType: SuperTypeExtractor = DummySuperTypeExtractor
  type ThisType = DummyType.type
  val ThisType: ThisTypeExtractor = DummyThisTypeExtractor
  type TypeBounds = DummyType.type
  val TypeBounds: TypeBoundsExtractor = DummyTypeBoundsExtractor
  type TypeRef = DummyType.type
  val TypeRef: TypeRefExtractor = DummyTypeRefExtractor
  val WildcardType: Type = DummyType
  def appliedType(tycon: Type,args: List[Type]): Type = DummyType
  def existentialAbstraction(tparams: List[Symbol],tpe0: Type): Type = DummyType
  def glb(ts: List[Type]): Type = DummyType
  def intersectionType(tps: List[Type],owner: Symbol): Type = DummyType
  def intersectionType(tps: List[Type]): Type = DummyType
  def lub(xs: List[Type]): Type = DummyType
  def polyType(tparams: List[Symbol],tpe: Type): Type = DummyType
  def refinedType(parents: List[Type],owner: Symbol): Type = DummyType
  def refinedType(parents: List[Type],owner: Symbol,decls: Scope,pos: Position): Type = DummyType
  def singleType(pre: Type,sym: Symbol): Type = DummyType
  def typeRef(pre: Type,sym: Symbol,args: List[Type]): Type = DummyType
  object DummyType extends AbsType {
    def =:=(that: Type): Boolean = notSupported()
    def <:<(that: Type): Boolean = notSupported()
    def asSeenFrom(pre: Type,clazz: Symbol): Type = notSupported()
    def baseClasses: List[Symbol] = notSupported()
    def baseType(clazz: Symbol): Type = notSupported()
    def contains(sym: Symbol): Boolean = notSupported()
    def declaration(name: Name): Symbol = notSupported()
    def declarations: Iterable[Symbol] = notSupported()
    def erasure: Type = notSupported()
    def exists(p: Type => Boolean): Boolean = notSupported()
    def find(p: Type => Boolean): Option[Type] = notSupported()
    def foreach(f: Type => Unit): Unit = notSupported()
    def isConcrete: Boolean = notSupported()
    def isHigherKinded: Boolean = notSupported()
    def isSpliceable: Boolean = notSupported()
    def kind: String = notSupported()
    def map(f: Type => Type): Type = notSupported()
    def member(name: Name): Symbol = notSupported()
    def members: Iterable[Symbol] = notSupported()
    def nonPrivateMember(name: Name): Symbol = notSupported()
    def nonPrivateMembers: Iterable[Symbol] = notSupported()
    def normalize: Type = notSupported()
    def parents: List[Type] = notSupported()
    def substituteTypes(from: List[Symbol],to: List[Type]): Type = notSupported()
    def typeArguments: List[Type] = notSupported()
    def typeConstructor: Type = notSupported()
    def typeParams: List[Symbol] = notSupported()
    def typeSymbol: Symbol = notSupported()
    def underlying: Type = notSupported()
    def widen: Type = notSupported()
  }
  object DummyAnnotatedTypeExtractor extends AnnotatedTypeExtractor {
    def apply(annotations: List[AnnotationInfo], underlying: Type, selfsym: Symbol): AnnotatedType = DummyType
    def unapply(tpe: AnnotatedType): Option[(List[AnnotationInfo], Type, Symbol)] = notSupported()
  }
  object DummyBoundedWildcardTypeExtractor extends BoundedWildcardTypeExtractor {
    def apply(bounds: TypeBounds): BoundedWildcardType = DummyType
    def unapply(tpe: BoundedWildcardType): Option[TypeBounds] = notSupported()
  }
  object DummyClassInfoTypeExtractor extends ClassInfoTypeExtractor {
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): ClassInfoType = DummyType
    def unapply(tpe: ClassInfoType): Option[(List[Type], Scope, Symbol)] = notSupported()
  }
  object DummyConstantTypeExtractor extends ConstantTypeExtractor {
    def apply(value: Constant): ConstantType = DummyType
    def unapply(tpe: ConstantType): Option[Constant] = notSupported()
  }
  object DummyExistentialTypeExtractor extends ExistentialTypeExtractor {
    def apply(quantified: List[Symbol], underlying: Type): ExistentialType = DummyType
    def unapply(tpe: ExistentialType): Option[(List[Symbol], Type)] = notSupported()
  }
  object DummyMethodTypeExtractor extends MethodTypeExtractor {
    def apply(params: List[Symbol], resultType: Type): MethodType = DummyType
    def unapply(tpe: MethodType): Option[(List[Symbol], Type)] = notSupported()
  }
  object DummyNullaryMethodTypeExtractor extends NullaryMethodTypeExtractor {
    def apply(resultType: Type): NullaryMethodType = DummyType
    def unapply(tpe: NullaryMethodType): Option[(Type)] = notSupported()
  }
  object DummyPolyTypeExtractor extends PolyTypeExtractor {
    def apply(typeParams: List[Symbol], resultType: Type): PolyType = DummyType
    def unapply(tpe: PolyType): Option[(List[Symbol], Type)] = notSupported()
  }
  object DummyRefinedTypeExtractor extends RefinedTypeExtractor {
    def apply(parents: List[Type], decls: Scope): RefinedType = DummyType
    def apply(parents: List[Type], decls: Scope, clazz: Symbol): RefinedType = DummyType
    def unapply(tpe: RefinedType): Option[(List[Type], Scope)] = notSupported()
  }
  object DummySingleTypeExtractor extends SingleTypeExtractor {
    def apply(pre: Type, sym: Symbol): Type = DummyType
    def unapply(tpe: SingleType): Option[(Type, Symbol)] = notSupported()
  }
  object DummySuperTypeExtractor extends SuperTypeExtractor {
    def apply(thistpe: Type, supertpe: Type): Type = DummyType
    def unapply(tpe: SuperType): Option[(Type, Type)] = notSupported()
  }
  object DummyThisTypeExtractor extends ThisTypeExtractor {
    def apply(sym: Symbol): Type = DummyType
    def unapply(tpe: ThisType): Option[Symbol] = notSupported()
  }
  object DummyTypeBoundsExtractor extends TypeBoundsExtractor {
    def apply(lo: Type, hi: Type): TypeBounds = DummyType
    def unapply(tpe: TypeBounds): Option[(Type, Type)] = notSupported()
  }
  object DummyTypeRefExtractor extends TypeRefExtractor {
    def apply(pre: Type, sym: Symbol, args: List[Type]): Type = DummyType
    def unapply(tpe: TypeRef): Option[(Type, Symbol, List[Type])] = notSupported()
  }

  // Utils
  def notSupported() = {
    throw new UnsupportedOperationException("Scala reflection not available on this platform." + mirrorDiagnostics(cl))
  }
}