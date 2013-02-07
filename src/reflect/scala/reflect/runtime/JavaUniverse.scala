package scala.reflect
package runtime

import internal.{SomePhase, NoPhase, Phase, TreeGen}

/** An implementation of [[scala.reflect.api.Universe]] for runtime reflection using JVM classloaders.
 *
 *  Should not be instantiated directly, use [[scala.reflect.runtime.universe]] instead.
 *
 *  @contentDiagram hideNodes "*Api" "*Extractor"
 */
class JavaUniverse extends internal.SymbolTable with ReflectSetup with runtime.SymbolTable { self =>

  def picklerPhase = SomePhase

  lazy val settings = new Settings
  def forInteractive = false
  def forScaladoc = false

  def log(msg: => AnyRef): Unit = if (settings.debug.value) println(" [] "+msg)

  type TreeCopier = InternalTreeCopierOps
  def newStrictTreeCopier: TreeCopier = new StrictTreeCopier
  def newLazyTreeCopier: TreeCopier = new LazyTreeCopier

  init()

  def init() {
    definitions.init()

    // workaround for http://groups.google.com/group/scala-internals/browse_thread/thread/97840ba4fd37b52e
    // constructors are by definition single-threaded, so we initialize all lazy vals (and local object) in advance
    // in order to avoid deadlocks later (e.g. one thread holds a global reflection lock and waits for definitions.Something to initialize,
    // whereas another thread holds a definitions.Something initialization lock and needs a global reflection lock to complete the initialization)

    // the list of lzycompute methods is mined by a simple python script
    // #!/usr/bin/env python
    // import os, sys, re, glob, fnmatch
    // from subprocess import check_call, check_output
    // root = check_output(["git", "rev-parse", "--show-toplevel"]).strip()
    // root = os.path.join(root, "build/quick/classes/reflect/scala/reflect")
    // classfiles = []
    // for container, dirnames, filenames in os.walk(root):
    //   for filename in fnmatch.filter(filenames, '*.class'):
    //     classfiles.append(os.path.join(container, filename))
    // for classfile in classfiles:
    //   blob = open(classfile).read()
    //   for found in re.findall(r"[\w\$]+\$lzycompute", blob):
    //     print found, "in", classfile[len(root) + 1:]


    // ======== Exprs ========
    // NOFORCE: tree$lzycompute in api/Exprs$ExprImpl.class
    // NOFORCE: staticType$lzycompute in api/Exprs$ExprImpl.class
    // NOFORCE: value$lzycompute in api/Exprs$ExprImpl.class
    // FORCE: Expr$lzycompute in api/Universe.class
    // FORCE: FixedMirrorTreeCreator$lzycompute in internal/SymbolTable.class
    // probably not used by runtime reflection, but better be safe than sorry
    Expr
    FixedMirrorTreeCreator

    // ======== Type tags ========
    // FORCE: WeakTypeTag$lzycompute in api/Universe.class
    // FORCE: TypeTag$lzycompute in api/Universe.class
    // NOFORCE: tpe$lzycompute in api/TypeTags$WeakTypeTagImpl.class
    // TODO: validate that scala-reflect never uses TypeTags itself (we have StdTags, but they are in compiler, so it should be okay)
    // FORCE: tpe$lzycompute in api/TypeTags$PredefTypeTag.class
    // FORCE: FixedMirrorTypeCreator$lzycompute in internal/SymbolTable.class
    // probably not used by runtime reflection, but better be safe than sorry
    WeakTypeTag
    TypeTag
    TypeTag.Byte.tpe
    TypeTag.Short.tpe
    TypeTag.Char.tpe
    TypeTag.Int.tpe
    TypeTag.Long.tpe
    TypeTag.Float.tpe
    TypeTag.Double.tpe
    TypeTag.Boolean.tpe
    TypeTag.Unit.tpe
    TypeTag.Any.tpe
    TypeTag.AnyVal.tpe
    TypeTag.AnyRef.tpe
    TypeTag.Object.tpe
    TypeTag.Nothing.tpe
    TypeTag.Null.tpe
    FixedMirrorTypeCreator

    // ======== Annotations ========
    // NOFORCE: isTrivial$lzycompute in internal/AnnotationInfos$AnnotationInfo.class
    // looks like isTrivial just calls into Type.isTrivial, which cannot go back to the calling annotationinfo, so we're safe from deadlocks
    // NOFORCE: forcedInfo$lzycompute in internal/AnnotationInfos$LazyAnnotationInfo.class
    // I can't immediately see that the completer cannot call into one of the methods of the calling AnnotationInfo
    // TODO: leaving this as a todo and for now we're at a mercy of chance
    // NOFORCE: encodedBytes$lzycompute in internal/AnnotationInfos$ScalaSigBytes.class
    // NOFORCE: sevenBitsMayBeZero$lzycompute in internal/AnnotationInfos$ScalaSigBytes.class
    // these are just bit/byte twiddling, they don't call into reflection at all
    // FORCE: UnmappableAnnotArg$lzycompute in internal/SymbolTable.class
    // FORCE: LiteralAnnotArg$lzycompute in internal/SymbolTable.class
    // FORCE: ArrayAnnotArg$lzycompute in internal/SymbolTable.class
    // FORCE: NestedAnnotArg$lzycompute in internal/SymbolTable.class
    // FORCE: ScalaSigBytes$lzycompute in internal/SymbolTable.class
    // FORCE: AnnotationInfo$lzycompute in internal/SymbolTable.class
    // FORCE: Annotation$lzycompute in internal/SymbolTable.class
    // FORCE: UnmappableAnnotation$lzycompute in internal/SymbolTable.class
    // FORCE: ThrownException$lzycompute in internal/SymbolTable.class
    UnmappableAnnotArg
    LiteralAnnotArg
    ArrayAnnotArg
    NestedAnnotArg
    ScalaSigBytes
    AnnotationInfo
    Annotation
    UnmappableAnnotation
    ThrownException

    // ======== Importers ========
    // NOFORCE: symMap$lzycompute in internal/Importers$StandardImporter.class
    // NOFORCE: tpeMap$lzycompute in internal/Importers$StandardImporter.class
    // NOFORCE: fixups$lzycompute in internal/Importers$StandardImporter.class
    // NOFORCE: reverse$lzycompute in internal/Importers$StandardImporter.class
    // these are simple constructors, which don't call into reflection
    // actually the first three of those could be non-lazy

    // ======== Mirrors ========
    // NOFORCE: RootPackage$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: RootClass$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: EmptyPackage$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: EmptyPackageClass$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: RootPackage$lzycompute in internal/Mirrors$Roots.class
    // NOFORCE: RootClass$lzycompute in internal/Mirrors$Roots.class
    // NOFORCE: EmptyPackage$lzycompute in internal/Mirrors$Roots.class
    // NOFORCE: EmptyPackageClass$lzycompute in internal/Mirrors$Roots.class
    // NOFORCE: unpickler$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: rootLoader$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // these are already forced in JavaMirror.init()
    // NOFORCE: universe$lzycompute in runtime/package$.class
    // that's the scala.reflect.runtime.universe
    // FORCE: 5scala$reflect$runtime$JavaMirrors$$mirrors$lzycompute in runtime/JavaUniverse.class
    rootMirror
    mirrors

    // ======== Names ========
    // FORCE: tpnme$lzycompute in internal/SymbolTable.class
    // FORCE: fulltpnme$lzycompute in internal/SymbolTable.class
    // FORCE: binarynme$lzycompute in internal/SymbolTable.class
    // FORCE: nme$lzycompute in internal/SymbolTable.class
    // FORCE: sn$lzycompute in internal/SymbolTable.class
    // FORCE: raw$lzycompute in internal/StdNames$TermNames.class
    // FORCE: Ascala$reflect$internal$SymbolTable$$SimpleNameOrdering$lzycompute in internal/SymbolTable.class
    // FORCE: 6scala$reflect$internal$StdNames$$compactify$lzycompute in internal/SymbolTable.class
    // probably most of these are already initialized by definitions.init()
    // but better be safe than sorry
    // NOFORCE: GetClassLoader$lzycompute in internal/StdNames$MSILNames.class
    // MSIL has been eol'd. probably we shouldn't care anymore
    // runtime reflection doesn't support MSIL anyways
    tpnme
    fulltpnme
    binarynme
    nme
    sn
    nme.raw
    lowPriorityNameOrdering
    nme.flattenedName()

    // ======== Attachments ========
    // FORCE: BackquotedIdentifierAttachment$lzycompute in internal/SymbolTable.class
    // FORCE: CompoundTypeTreeOriginalAttachment$lzycompute in internal/SymbolTable.class
    // FORCE: MacroExpansionAttachment$lzycompute in internal/SymbolTable.class
    // FORCE: SuppressMacroExpansionAttachment$lzycompute in internal/SymbolTable.class
    // probably not used by runtime reflection
    // but better be safe than sorry
    BackquotedIdentifierAttachment
    CompoundTypeTreeOriginalAttachment
    MacroExpansionAttachment
    SuppressMacroExpansionAttachment

    // ======== TreeInfo ========
    // NOFORCE: DynamicUpdate$lzycompute in internal/TreeInfo.class
    // NOFORCE: DynamicApplication$lzycompute in internal/TreeInfo.class
    // NOFORCE: DynamicApplicationNamed$lzycompute in internal/TreeInfo.class
    // NOFORCE: MacroImplReference$lzycompute in internal/TreeInfo.class
    // NOFORCE: StripCast$lzycompute in internal/TreeInfo.class
    // NOFORCE: Applied$lzycompute in internal/TreeInfo.class
    // NOFORCE: IsTrue$lzycompute in internal/TreeInfo.class
    // NOFORCE: IsFalse$lzycompute in internal/TreeInfo.class
    // NOFORCE: IsIf$lzycompute in internal/TreeInfo.class
    // TreeInfo is never instantiated by runtime reflection

    // ======== Miscellaneous ========
    // FORCE: BooleanFlag$lzycompute in api/Universe.class
    // FORCE: traceSymbols$lzycompute in internal/SymbolTable.class
    // FORCE: perRunCaches$lzycompute in internal/SymbolTable.class
    // FORCE: typeDebug$lzycompute in internal/SymbolTable.class
    // FORCE: str$lzycompute in internal/TypeDebugging$typeDebug$.class
    // FORCE: ConsoleWriter$lzycompute in internal/SymbolTable.class
    // FORCE: settings$lzycompute in runtime/JavaUniverse.class
    // NOFORCE: Lscala$reflect$internal$util$TraceSymbolActivity$$findErasurePhase$lzycompute in internal/SymbolTable$traceSymbols$.class
    // NOFORCE: lineIndices$lzycompute in internal/util/BatchSourceFile.class
    // NOFORCE: extension$lzycompute in io/AbstractFile.class
    // NOFORCE: in_s$lzycompute in io/File.class
    // NOFORCE: out_s$lzycompute in io/File.class
    // NOFORCE: in$lzycompute in io/File.class
    // NOFORCE: out$lzycompute in io/File.class
    // NOFORCE: in$lzycompute in io/Streamable$Bytes$class.class
    // noforced guys don't call into reflection, so even when not initialized they don't pose a deadlock threat
    // FORCE: Scope$lzycompute in internal/SymbolTable.class
    // FORCE: EmptyScope$lzycompute in internal/SymbolTable.class
    // FORCE: Flag$lzycompute in internal/SymbolTable.class
    // FORCE: KindErrors$lzycompute in internal/SymbolTable.class
    BooleanFlag
    traceSymbols
    perRunCaches
    typeDebug
    typeDebug.str
    ConsoleWriter
    settings
    Scope
    EmptyScope
    Flag
    KindErrors

    // ======== Transforms ========
    // NOFORCE: ObjectArray$lzycompute in internal/transform/Erasure$ErasureMap.class
    // NOFORCE: ErasedObject$lzycompute in internal/transform/Erasure$ErasureMap.class
    // too much hassle to force those, and they cannot be called back from reflection => safe from deadlocks
    // FORCE: GenericArray$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    // FORCE: scalaErasure$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    // FORCE: specialScalaErasure$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    // FORCE: javaErasure$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    // FORCE: verifiedJavaErasure$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    // FORCE: boxingErasure$lzycompute in internal/transform/Transforms$$anonfun$3$$anon$1.class
    refChecks
    uncurry
    erasure
    erasure.GenericArray
    erasure.scalaErasure
    erasure.specialScalaErasure
    erasure.javaErasure
    erasure.verifiedJavaErasure
    erasure.boxingErasure

    // ======== Symbols ========
    // FORCE: NoSymbol$lzycompute in internal/SymbolTable.class
    // FORCE: 6scala$reflect$internal$Symbols$$TypeHistory$lzycompute in internal/SymbolTable.class
    // FORCE: SymbolKind$lzycompute in internal/Symbols$Symbol.class
    NoSymbol
    definitions.AnyRefClass.info
    SymbolKind

    // ======== Trees ========
    // FORCE: PackageDef$lzycompute in internal/SymbolTable.class
    // FORCE: ClassDef$lzycompute in internal/SymbolTable.class
    // FORCE: ModuleDef$lzycompute in internal/SymbolTable.class
    // FORCE: ValDef$lzycompute in internal/SymbolTable.class
    // FORCE: DefDef$lzycompute in internal/SymbolTable.class
    // FORCE: TypeDef$lzycompute in internal/SymbolTable.class
    // FORCE: LabelDef$lzycompute in internal/SymbolTable.class
    // FORCE: ImportSelector$lzycompute in internal/SymbolTable.class
    // FORCE: Import$lzycompute in internal/SymbolTable.class
    // FORCE: Template$lzycompute in internal/SymbolTable.class
    // FORCE: Block$lzycompute in internal/SymbolTable.class
    // FORCE: CaseDef$lzycompute in internal/SymbolTable.class
    // FORCE: Alternative$lzycompute in internal/SymbolTable.class
    // FORCE: Star$lzycompute in internal/SymbolTable.class
    // FORCE: Bind$lzycompute in internal/SymbolTable.class
    // FORCE: UnApply$lzycompute in internal/SymbolTable.class
    // FORCE: ArrayValue$lzycompute in internal/SymbolTable.class
    // FORCE: Function$lzycompute in internal/SymbolTable.class
    // FORCE: Assign$lzycompute in internal/SymbolTable.class
    // FORCE: AssignOrNamedArg$lzycompute in internal/SymbolTable.class
    // FORCE: If$lzycompute in internal/SymbolTable.class
    // FORCE: Match$lzycompute in internal/SymbolTable.class
    // FORCE: Return$lzycompute in internal/SymbolTable.class
    // FORCE: Try$lzycompute in internal/SymbolTable.class
    // FORCE: Throw$lzycompute in internal/SymbolTable.class
    // FORCE: New$lzycompute in internal/SymbolTable.class
    // FORCE: Typed$lzycompute in internal/SymbolTable.class
    // FORCE: TypeApply$lzycompute in internal/SymbolTable.class
    // FORCE: Apply$lzycompute in internal/SymbolTable.class
    // FORCE: ApplyDynamic$lzycompute in internal/SymbolTable.class
    // FORCE: Super$lzycompute in internal/SymbolTable.class
    // FORCE: This$lzycompute in internal/SymbolTable.class
    // FORCE: Select$lzycompute in internal/SymbolTable.class
    // FORCE: Ident$lzycompute in internal/SymbolTable.class
    // FORCE: ReferenceToBoxed$lzycompute in internal/SymbolTable.class
    // FORCE: Literal$lzycompute in internal/SymbolTable.class
    // FORCE: Annotated$lzycompute in internal/SymbolTable.class
    // FORCE: SingletonTypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: SelectFromTypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: CompoundTypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: AppliedTypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: TypeBoundsTree$lzycompute in internal/SymbolTable.class
    // FORCE: ExistentialTypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: TypeTree$lzycompute in internal/SymbolTable.class
    // FORCE: Modifiers$lzycompute in internal/SymbolTable.class
    // FORCE: Constant$lzycompute in internal/SymbolTable.class
    // FORCE: treeBuild$lzycompute in internal/SymbolTable.class
    // FORCE: posAssigner$lzycompute in internal/SymbolTable.class
    // FORCE: NoMods$lzycompute in api/Universe.class
    // FORCE: EmptyTree$lzycompute in internal/SymbolTable.class
    // FORCE: emptyValDef$lzycompute in internal/SymbolTable.class
    // FORCE: EmptyTreeTypeSubstituter$lzycompute in internal/SymbolTable.class
    // FORCE: 3scala$reflect$internal$Trees$$duplicator$lzycompute in internal/SymbolTable.class
    PackageDef
    ClassDef
    ModuleDef
    ValDef
    DefDef
    TypeDef
    LabelDef
    ImportSelector
    Import
    Template
    Block
    CaseDef
    Alternative
    Star
    Bind
    UnApply
    ArrayValue
    Function
    Assign
    AssignOrNamedArg
    If
    Match
    Return
    Try
    Throw
    New
    Typed
    TypeApply
    Apply
    ApplyDynamic
    Super
    This
    Select
    Ident
    ReferenceToBoxed
    Literal
    Annotated
    SingletonTypeTree
    SelectFromTypeTree
    CompoundTypeTree
    AppliedTypeTree
    TypeBoundsTree
    ExistentialTypeTree
    TypeTree
    Modifiers
    Constant
    treeBuild
    posAssigner
    NoMods
    EmptyTree
    emptyValDef
    EmptyTreeTypeSubstituter
    Literal(Constant(42)).duplicate

    // ======== Types ========
    // FORCE: undoLog$lzycompute in internal/SymbolTable.class
    // FORCE: UnmappableTree$lzycompute in internal/SymbolTable.class
    // FORCE: NotNullType$lzycompute in internal/SymbolTable.class
    // FORCE: ErrorType$lzycompute in internal/SymbolTable.class
    // FORCE: WildcardType$lzycompute in internal/SymbolTable.class
    // FORCE: BoundedWildcardType$lzycompute in internal/SymbolTable.class
    // FORCE: NoType$lzycompute in internal/SymbolTable.class
    // FORCE: NoPrefix$lzycompute in internal/SymbolTable.class
    // FORCE: ThisType$lzycompute in internal/SymbolTable.class
    // FORCE: SingleType$lzycompute in internal/SymbolTable.class
    // FORCE: SuperType$lzycompute in internal/SymbolTable.class
    // FORCE: TypeBounds$lzycompute in internal/SymbolTable.class
    // NOFORCE: annotationArgRewriter$1$lzycompute in internal/Types$AsSeenFromMap.class
    // local object => not a problem
    // FORCE: RefinedType$lzycompute in internal/SymbolTable.class
    // FORCE: ClassInfoType$lzycompute in internal/SymbolTable.class
    // FORCE: ConstantType$lzycompute in internal/SymbolTable.class
    // FORCE: TypeRef$lzycompute in internal/SymbolTable.class
    // FORCE: MethodType$lzycompute in internal/SymbolTable.class
    // FORCE: NullaryMethodType$lzycompute in internal/SymbolTable.class
    // FORCE: PolyType$lzycompute in internal/SymbolTable.class
    // FORCE: ExistentialType$lzycompute in internal/SymbolTable.class
    // FORCE: OverloadedType$lzycompute in internal/SymbolTable.class
    // FORCE: AntiPolyType$lzycompute in internal/SymbolTable.class
    // FORCE: HasTypeMember$lzycompute in internal/SymbolTable.class
    // FORCE: HasTypeParams$lzycompute in internal/SymbolTable.class
    // FORCE: TypeVar$lzycompute in internal/SymbolTable.class
    // FORCE: AnnotatedType$lzycompute in internal/SymbolTable.class
    // FORCE: NamedType$lzycompute in internal/SymbolTable.class
    // FORCE: DeBruijnIndex$lzycompute in internal/SymbolTable.class
    // FORCE: DeBruijnBinder$lzycompute in internal/SymbolTable.class
    // FORCE: ErasedValueType$lzycompute in internal/SymbolTable.class
    // FORCE: GenPolyType$lzycompute in internal/SymbolTable.class
    // FORCE: normalizeAliases$lzycompute in internal/SymbolTable.class
    // FORCE: dropSingletonType$lzycompute in internal/SymbolTable.class
    // FORCE: dropAllRefinements$lzycompute in internal/SymbolTable.class
    // FORCE: dropRepeatedParamType$lzycompute in internal/SymbolTable.class
    // FORCE: toDeBruijn$lzycompute in internal/SymbolTable.class
    // FORCE: numericLoBound$lzycompute in internal/SymbolTable.class
    // FORCE: numericHiBound$lzycompute in internal/SymbolTable.class
    // FORCE: TypeConstraint$lzycompute in internal/SymbolTable.class
    // FORCE: unwrapToClass$lzycompute in internal/SymbolTable.class
    // FORCE: unwrapToStableClass$lzycompute in internal/SymbolTable.class
    // FORCE: unwrapWrapperTypes$lzycompute in internal/SymbolTable.class
    // FORCE: IsDependentCollector$lzycompute in internal/SymbolTable.class
    // FORCE: ApproximateDependentMap$lzycompute in internal/SymbolTable.class
    // FORCE: StripAnnotationsMap$lzycompute in internal/SymbolTable.class
    // FORCE: wildcardToTypeVarMap$lzycompute in internal/SymbolTable.class
    // FORCE: typeVarToOriginMap$lzycompute in internal/SymbolTable.class
    // FORCE: ErroneousCollector$lzycompute in internal/SymbolTable.class
    // NOFORCE: scala$reflect$internal$Types$$commonOwnerMapObj$lzycompute in internal/SymbolTable.class
    // not used in runtime reflection, overridden to provide thread-safety
    // FORCE: RecoverableCyclicReference$lzycompute in internal/SymbolTable.class
    // FORCE: CyclicReference$lzycompute in internal/SymbolTable.class
    // NOFORCE: scala$reflect$internal$Types$ClassInfoType$$enterRefs$lzycompute in internal/Types$ClassInfoType.class
    // not used in runtime reflection, only called by typer
    // NOFORCE: Jscala$reflect$internal$Types$InstantiateDependentMap$$StableArg$lzycompute in internal/Types$InstantiateDependentMap.class
    // NOFORCE: Dscala$reflect$internal$Types$InstantiateDependentMap$$Arg$lzycompute in internal/Types$InstantiateDependentMap.class
    // don't call into reflection
    // NOFORCE: trans$1$lzycompute in internal/Types$SubstSymMap.class
    // NOFORCE: trans$2$lzycompute in internal/Types$SubstTypeMap.class
    // NOFORCE: treeTrans$1$lzycompute in internal/Types$InstantiateDependentMap.class
    // local objects => we're fine
    // FORCE: adaptToNewRunMap$lzycompute in internal/SymbolTable.class
    // NOFORCE?! maxDepth$lzycompute in internal/BaseTypeSeqs$BaseTypeSeq.class
    // NOFORCE?! maxDepth$lzycompute in runtime/SynchronizedOps$$anon$4.class
    // NOFORCE?! maxDepth$lzycompute in runtime/SynchronizedOps$SynchronizedBaseTypeSeq$$anon$3.class
    // it's totally not obvious whether typeDepth, which is called by maxDepth,
    // can or cannot indirectly call into one of the methods of the calling BaseTypeSeq
    // TODO: leaving this for further investigation risking to get a deadlock for now
    undoLog
    UnmappableTree
    NotNullType
    ErrorType
    WildcardType
    BoundedWildcardType
    NoType
    NoPrefix
    ThisType
    SingleType
    SuperType
    TypeBounds
    RefinedType
    ClassInfoType
    ConstantType
    TypeRef
    MethodType
    NullaryMethodType
    PolyType
    ExistentialType
    OverloadedType
    AntiPolyType
    HasTypeMember
    HasTypeParams
    TypeVar
    AnnotatedType
    NamedType
    DeBruijnIndex
    DeBruijnBinder
    ErasedValueType
    GenPolyType
    normalizeAliases
    dropSingletonType
    dropAllRefinements
    dropRepeatedParamType
    toDeBruijn
    numericLoBound
    numericHiBound
    TypeConstraint
    unwrapToClass
    unwrapToStableClass
    unwrapWrapperTypes
    IsDependentCollector
    ApproximateDependentMap
    StripAnnotationsMap
    wildcardToTypeVarMap
    typeVarToOriginMap
    ErroneousCollector
    RecoverableCyclicReference
    CyclicReference
    adaptToNewRunMap

    // ======== JavaMirrors ========
    // NOFORCE: assocs$lzycompute in runtime/JavaMirrors$JavaMirror$JavaAnnotationProxy.class
    // NOFORCE: jconstr$lzycompute in runtime/JavaMirrors$JavaMirror$JavaConstructorMirror.class
    // NOFORCE: jfield$lzycompute in runtime/JavaMirrors$JavaMirror$JavaFieldMirror.class
    // NOFORCE: jmeth$lzycompute in runtime/JavaMirrors$JavaMirror$JavaMethodMirror.class
    // NOFORCE: signature$lzycompute in runtime/JavaMirrors$JavaMirror$JavaTemplateMirror.class
    // NOFORCE: PrimitiveClass$lzycompute in runtime/JavaMirrors$JavaMirror$toAnnotArg$.class
    // NOFORCE: EnumClass$lzycompute in runtime/JavaMirrors$JavaMirror$toAnnotArg$.class
    // NOFORCE: ArrayClass$lzycompute in runtime/JavaMirrors$JavaMirror$toAnnotArg$.class
    // NOFORCE: AnnotationClass$lzycompute in runtime/JavaMirrors$JavaMirror$toAnnotArg$.class
    // NOFORCE: ConstantArg$lzycompute in runtime/JavaMirrors$JavaMirror$toAnnotArg$.class
    // NOFORCE: Cscala$reflect$runtime$JavaMirrors$JavaMirror$$toAnnotArg$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: Lscala$reflect$runtime$JavaMirrors$JavaMirror$$JavaAnnotationProxy$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: Bscala$reflect$runtime$TwoWayCaches$TwoWayCache$$SomeRef$lzycompute in runtime/TwoWayCaches$TwoWayCache.class
    // NOFORCE: bytecodelessMethodOwners$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // NOFORCE: bytecodefulObjectMethods$lzycompute in runtime/JavaMirrors$JavaMirror.class
    // functionality from JavaMirrors cannot be called from reflect.internal, so there's no danger of deadlock

    // ======== Locks ========
    // locks in runtime reflection look like `lazy val XXXlock = new Object`
    // since initializers don't call into reflection, there's no problem in not forcing those lazy vals here

    // ======== Definitions ========
    // FORCE: definitions$lzycompute in internal/SymbolTable.class
    // FORCE: JavaLangPackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaLangPackageClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaPackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaPackageClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RuntimePackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RuntimePackageClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaLangEnumClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: anyparam$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: anyvalparam$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: anyrefparam$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyRefClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ObjectClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyRefTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ObjectTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyRefModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyValClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnyValTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RuntimeNothingClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RuntimeNullClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NothingClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NullClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NothingTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NullTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassCastExceptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IndexOutOfBoundsExceptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: InvocationTargetExceptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MatchErrorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NonLocalReturnControlClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: $NullPointerExceptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ThrowableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UninitializedErrorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PartialFunctionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AbstractPartialFunctionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SymbolClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DynamicClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SysPackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UnqualifiedModules$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UnqualifiedOwners$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PredefModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PredefModuleClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SpecializableModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: GroupOfSpecializable$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ConsoleModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaRunTimeModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SymbolModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Symbol_apply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringAddClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ArrowAssocClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringAdd_$plus$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NotNullClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaNumberClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TraitSetterAnnotationClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DelayedInitClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TypeConstraintClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SingletonClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SerializableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaSerializableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ComparableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: CloneableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaCloneableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaNumberClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RemoteInterfaceClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RemoteExceptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ByNameParamClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: EqualsPatternClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaRepeatedParamClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RepeatedParamClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MatchingStrategyClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ConsClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IterableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IteratorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ListClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SeqClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringBuilderClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TraversableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ListModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: List_apply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NilModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SeqModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IteratorModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Iterator_apply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ArrayModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ArrayModule_overloadedApply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ArrayClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Array_apply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Array_update$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Array_length$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Array_clone$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SoftReferenceClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: WeakReferenceClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MethodClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: EmptyMethodCacheClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MethodCacheClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ReflectPackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ReflectApiPackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ReflectRuntimePackage$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PartialManifestClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PartialManifestModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FullManifestClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FullManifestModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: OptManifestClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NoManifest$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ExprsClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ExprClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ExprModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassTagModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassTagClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TypeTagsClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: WeakTypeTagClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: WeakTypeTagModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TypeTagClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TypeTagModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ApiUniverseClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: JavaUniverseClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MirrorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TypeCreatorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TreeCreatorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MacroContextClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MacroImplAnnotation$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StringContextClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaSignatureAnnotation$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaLongSignatureAnnotation$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: OptionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: OptionModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Option_apply$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SomeClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NoneModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SomeModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ProductClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TupleClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FunctionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AbstractFunctionClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ProductRootClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ObjectArray$lzycompute in internal/Definitions$DefinitionsClass.class
    // NOFORCE: ComparatorClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // NOFORCE: ValueTypeClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // NOFORCE: DelegateClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // NOFORCE: Delegate_scalaCallerTargets$lzycompute in internal/Definitions$DefinitionsClass.class
    // MSIL has been eol'd. probably we shouldn't care anymore
    // runtime reflection doesn't support MSIL anyways
    // FORCE: Any_$eq$eq$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_$bang$eq$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_equals$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_hashCode$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_toString$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_$hash$hash$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_getClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_isInstanceOf$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Any_asInstanceOf$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_$hash$hash$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_$eq$eq$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_$bang$eq$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_eq$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_ne$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_isInstanceOf$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_asInstanceOf$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Object_synchronized$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: String_$plus$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ObjectRefClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: VolatileObjectRefClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RuntimeStaticsModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxesRunTimeModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxesRunTimeClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedNumberClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedCharacterClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedBooleanClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedByteClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedShortClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedIntClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedLongClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedFloatClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedDoubleClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Boxes_isNumberOrBool$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Boxes_isNumber$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedUnitClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BoxedUnitModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnnotationClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassfileAnnotationClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: StaticAnnotationClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BridgeClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ElidableMethodClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ImplicitNotFoundClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MigrationAnnotationClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaStrictFPAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SerializableAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SwitchClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TailrecClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: VarargsClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: uncheckedStableClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: uncheckedVarianceClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BeanPropertyAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BooleanBeanPropertyAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: CloneableAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: CompileTimeOnlyAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DeprecatedAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DeprecatedNameAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: $DeprecatedInheritanceAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DeprecatedOverridingAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: NativeAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: RemoteAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaInlineClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaNoInlineClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SerialVersionUIDAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SpecializedClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ThrowsClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: TransientAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UncheckedClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UnspecializedClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: VolatileAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BeanGetterTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BeanSetterTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FieldTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: GetterTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ParamTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: SetterTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ClassTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ObjectTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MethodTargetClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: LanguageFeatureAnnot$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: languageFeatureModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: experimentalModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: MacrosFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DynamicsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: PostfixOpsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ReflectiveCallsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ImplicitConversionsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: HigherKindsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ExistentialsFeature$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: metaAnnotations$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: AnnotationDefaultAttr$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: boxedClassValues$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: isUnbox$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: isBox$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: isPhantomClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: syntheticCoreClasses$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: syntheticCoreMethods$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: hijackedCoreClasses$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: symbolsNotPresentInBytecode$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: $isPossibleSyntheticParent$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: boxedValueClassesSet$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: abbrvTag$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: numericWeight$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: boxedModule$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: boxedClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: refClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: volatileRefClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: boxMethod$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: unboxMethod$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UnitClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ByteClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ShortClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: CharClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IntClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: LongClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FloatClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DoubleClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BooleanClass$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Boolean_and$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Boolean_or$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: Boolean_not$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: UnitTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ByteTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ShortTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: CharTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: IntTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: LongTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: FloatTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: DoubleTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: BooleanTpe$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaNumericValueClasses$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaValueClassesNoUnit$lzycompute in internal/Definitions$DefinitionsClass.class
    // FORCE: ScalaValueClasses$lzycompute in internal/Definitions$DefinitionsClass.class
    definitions.JavaLangPackage
    definitions.JavaLangPackageClass
    definitions.ScalaPackage
    definitions.ScalaPackageClass
    definitions.RuntimePackage
    definitions.RuntimePackageClass
    definitions.JavaLangEnumClass
    definitions.anyparam
    definitions.anyvalparam
    definitions.anyrefparam
    definitions.AnyClass
    definitions.AnyRefClass
    definitions.ObjectClass
    definitions.AnyTpe
    definitions.AnyRefTpe
    definitions.ObjectTpe
    // FIXME: definitions.AnyRefModule
    definitions.AnyValClass
    definitions.AnyValTpe
    definitions.RuntimeNothingClass
    definitions.RuntimeNullClass
    definitions.NothingClass
    definitions.NullClass
    definitions.NothingTpe
    definitions.NullTpe
    definitions.ClassCastExceptionClass
    definitions.IndexOutOfBoundsExceptionClass
    definitions.InvocationTargetExceptionClass
    definitions.MatchErrorClass
    definitions.NonLocalReturnControlClass
    definitions.NullPointerExceptionClass
    definitions.ThrowableClass
    definitions.UninitializedErrorClass
    definitions.PartialFunctionClass
    definitions.AbstractPartialFunctionClass
    definitions.SymbolClass
    definitions.StringClass
    definitions.StringModule
    definitions.ClassClass
    definitions.DynamicClass
    definitions.SysPackage
    definitions.UnqualifiedModules
    definitions.UnqualifiedOwners
    definitions.PredefModule
    definitions.PredefModuleClass
    definitions.SpecializableModule
    definitions.GroupOfSpecializable
    definitions.ConsoleModule
    definitions.ScalaRunTimeModule
    definitions.SymbolModule
    definitions.Symbol_apply
    definitions.StringAddClass
    definitions.ArrowAssocClass
    definitions.StringAdd_$plus
    definitions.NotNullClass
    definitions.ScalaNumberClass
    definitions.TraitSetterAnnotationClass
    definitions.DelayedInitClass
    definitions.TypeConstraintClass
    definitions.SingletonClass
    definitions.SerializableClass
    definitions.JavaSerializableClass
    definitions.ComparableClass
    definitions.CloneableClass
    definitions.JavaCloneableClass
    definitions.JavaNumberClass
    definitions.RemoteInterfaceClass
    definitions.RemoteExceptionClass
    definitions.ByNameParamClass
    definitions.EqualsPatternClass
    definitions.JavaRepeatedParamClass
    definitions.RepeatedParamClass
    // FIXME: definitions.MatchingStrategyClass
    definitions.ConsClass
    definitions.IterableClass
    definitions.IteratorClass
    definitions.ListClass
    definitions.SeqClass
    definitions.StringBuilderClass
    definitions.TraversableClass
    definitions.ListModule
    definitions.List_apply
    definitions.NilModule
    definitions.SeqModule
    definitions.IteratorModule
    definitions.Iterator_apply
    definitions.ArrayModule
    definitions.ArrayModule_overloadedApply
    definitions.ArrayClass
    definitions.Array_apply
    definitions.Array_update
    definitions.Array_length
    definitions.Array_clone
    definitions.SoftReferenceClass
    definitions.WeakReferenceClass
    definitions.MethodClass
    definitions.EmptyMethodCacheClass
    definitions.MethodCacheClass
    definitions.ReflectPackage
    definitions.ReflectApiPackage
    definitions.ReflectRuntimePackage
    definitions.PartialManifestClass
    definitions.PartialManifestModule
    definitions.FullManifestClass
    definitions.FullManifestModule
    definitions.OptManifestClass
    definitions.NoManifest
    definitions.ExprsClass
    definitions.ExprClass
    definitions.ExprModule
    definitions.ClassTagModule
    definitions.ClassTagClass
    definitions.TypeTagsClass
    definitions.WeakTypeTagClass
    definitions.WeakTypeTagModule
    definitions.TypeTagClass
    definitions.TypeTagModule
    definitions.ApiUniverseClass
    definitions.JavaUniverseClass
    definitions.MirrorClass
    definitions.TypeCreatorClass
    definitions.TreeCreatorClass
    definitions.MacroContextClass
    definitions.MacroImplAnnotation
    definitions.StringContextClass
    definitions.ScalaSignatureAnnotation
    definitions.ScalaLongSignatureAnnotation
    definitions.OptionClass
    definitions.OptionModule
    definitions.Option_apply
    definitions.SomeClass
    definitions.NoneModule
    definitions.SomeModule
    definitions.ProductClass
    definitions.TupleClass
    definitions.FunctionClass
    definitions.AbstractFunctionClass
    definitions.ProductRootClass
    definitions.ObjectArray
    definitions.Any_$eq$eq
    definitions.Any_$bang$eq
    definitions.Any_equals
    definitions.Any_hashCode
    definitions.Any_toString
    definitions.Any_$hash$hash
    definitions.Any_getClass
    definitions.Any_isInstanceOf
    definitions.Any_asInstanceOf
    definitions.Object_$hash$hash
    definitions.Object_$eq$eq
    definitions.Object_$bang$eq
    definitions.Object_eq
    definitions.Object_ne
    definitions.Object_isInstanceOf
    definitions.Object_asInstanceOf
    definitions.Object_synchronized
    definitions.String_$plus
    definitions.ObjectRefClass
    definitions.VolatileObjectRefClass
    definitions.RuntimeStaticsModule
    definitions.BoxesRunTimeModule
    definitions.BoxesRunTimeClass
    definitions.BoxedNumberClass
    definitions.BoxedCharacterClass
    definitions.BoxedBooleanClass
    definitions.BoxedByteClass
    definitions.BoxedShortClass
    definitions.BoxedIntClass
    definitions.BoxedLongClass
    definitions.BoxedFloatClass
    definitions.BoxedDoubleClass
    definitions.Boxes_isNumberOrBool
    definitions.Boxes_isNumber
    definitions.BoxedUnitClass
    definitions.BoxedUnitModule
    definitions.AnnotationClass
    definitions.ClassfileAnnotationClass
    definitions.StaticAnnotationClass
    definitions.BridgeClass
    definitions.ElidableMethodClass
    definitions.ImplicitNotFoundClass
    definitions.MigrationAnnotationClass
    definitions.ScalaStrictFPAttr
    definitions.SerializableAttr
    definitions.SwitchClass
    definitions.TailrecClass
    definitions.VarargsClass
    definitions.uncheckedStableClass
    definitions.uncheckedVarianceClass
    definitions.BeanPropertyAttr
    definitions.BooleanBeanPropertyAttr
    definitions.CloneableAttr
    definitions.CompileTimeOnlyAttr
    definitions.DeprecatedAttr
    definitions.DeprecatedNameAttr
    definitions.DeprecatedInheritanceAttr
    definitions.DeprecatedOverridingAttr
    definitions.NativeAttr
    definitions.RemoteAttr
    definitions.ScalaInlineClass
    definitions.ScalaNoInlineClass
    definitions.SerialVersionUIDAttr
    definitions.SpecializedClass
    definitions.ThrowsClass
    definitions.TransientAttr
    definitions.UncheckedClass
    definitions.UnspecializedClass
    definitions.VolatileAttr
    definitions.BeanGetterTargetClass
    definitions.BeanSetterTargetClass
    definitions.FieldTargetClass
    definitions.GetterTargetClass
    definitions.ParamTargetClass
    definitions.SetterTargetClass
    definitions.ClassTargetClass
    definitions.ObjectTargetClass
    definitions.MethodTargetClass
    definitions.LanguageFeatureAnnot
    definitions.languageFeatureModule
    definitions.experimentalModule
    definitions.MacrosFeature
    definitions.DynamicsFeature
    definitions.PostfixOpsFeature
    definitions.ReflectiveCallsFeature
    definitions.ImplicitConversionsFeature
    definitions.HigherKindsFeature
    definitions.ExistentialsFeature
    definitions.metaAnnotations
    definitions.AnnotationDefaultAttr
    definitions.boxedClassValues
    definitions.isUnbox
    definitions.isBox
    definitions.isPhantomClass
    definitions.syntheticCoreClasses
    definitions.syntheticCoreMethods
    definitions.hijackedCoreClasses
    definitions.symbolsNotPresentInBytecode
    definitions.isPossibleSyntheticParent
    definitions.boxedValueClassesSet
    definitions.abbrvTag
    definitions.numericWeight
    definitions.boxedModule
    definitions.boxedClass
    definitions.refClass
    definitions.volatileRefClass
    definitions.boxMethod
    definitions.unboxMethod
    definitions.UnitClass
    definitions.ByteClass
    definitions.ShortClass
    definitions.CharClass
    definitions.IntClass
    definitions.LongClass
    definitions.FloatClass
    definitions.DoubleClass
    definitions.BooleanClass
    definitions.Boolean_and
    definitions.Boolean_or
    definitions.Boolean_not
    definitions.UnitTpe
    definitions.ByteTpe
    definitions.ShortTpe
    definitions.CharTpe
    definitions.IntTpe
    definitions.LongTpe
    definitions.FloatTpe
    definitions.DoubleTpe
    definitions.BooleanTpe
    definitions.ScalaNumericValueClasses
    definitions.ScalaValueClassesNoUnit
    definitions.ScalaValueClasses
  }
}

