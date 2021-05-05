/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.tasty.bridge

import scala.annotation.tailrec
import scala.reflect.io.AbstractFile

import scala.tools.tasty.{TastyName, TastyFlags}, TastyFlags._, TastyName.ObjectName
import scala.tools.nsc.tasty.{TastyUniverse, TastyModes, SafeEq}, TastyModes._
import scala.reflect.internal.MissingRequirementError
import scala.collection.mutable

/**This contains the definition for `Context`, along with standard error throwing capabilities with user friendly
 * formatted errors that can change their output depending on the context mode.
 */
trait ContextOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  private def describeOwner(owner: Symbol): String = {
    val kind =
      if (owner.isOneOf(Param | ParamSetter)) {
        if (owner.isType) "type parameter"
        else "parameter"
      }
      else {
        owner.kindString
      }
    s"$kind ${owner.nameString}"
  }

  def boundsString(owner: Symbol): String = {
    if (owner.isType) s"bounds of $owner"
    else if (owner.isOneOf(Param | ParamSetter)) s"parameter $owner"
    else "result"
  }

  @inline final def unsupportedTermTreeError[T](noun: String)(implicit ctx: Context): T =
    unsupportedError(
      if (ctx.mode.is(ReadAnnotation)) s"$noun in an annotation of ${describeOwner(ctx.owner)}; note that complex trees are not yet supported for Annotations"
      else noun
    )

  @inline final def unsupportedError[T](noun: String)(implicit ctx: Context): T = {
    typeError(unsupportedMessage(noun))
  }

  @inline final def unsupportedMessage(noun: String)(implicit ctx: Context): String = {
    s"Unsupported Scala 3 $noun; found in ${location(ctx.globallyVisibleOwner)}."
  }

  final def location(owner: Symbol): String = {
    if (!isSymbol(owner)) "<NoSymbol>"
    else if (owner.isClass) s"${owner.kindString} ${owner.fullNameString}"
    else s"${describeOwner(owner)} in ${location(owner.owner)}"
  }

  @inline final def typeError[T](msg: String): T = throw new u.TypeError(msg)

  final def abortWith[T](msg: String): T = {
    u.assert(false, msg)
    ???
  }

  @inline final def assert(assertion: Boolean, msg: => Any): Unit =
    u.assert(assertion, msg)

  @inline final def assert(assertion: Boolean): Unit =
    u.assert(assertion, "")

  private final def findObject(owner: Symbol, name: u.Name): Symbol = {
    val scope =
      if (owner != null && owner.isClass) owner.rawInfo.decls
      else u.EmptyScope
    val it = scope.lookupAll(name).withFilter(_.isModule)
    if (it.hasNext) it.next()
    else u.NoSymbol //throw new AssertionError(s"no module $name in ${location(owner)}")
  }

  /**Perform an operation within a context that has the mode `IndexStats` will force any collected annotations
   * afterwards */
  def inIndexStatsContext(op: Context => Unit)(implicit ctx: Context): Unit = {
    val statsCtx = ctx.addMode(IndexStats)
    op(statsCtx)
    statsCtx.initialContext.forceAnnotations()
  }

  /** Perform an operation within a context that has the mode `InnerScope` will enter any inline methods afterwards */
  def inInnerScopeContext(op: Context => Unit)(implicit ctx: Context): Unit = {
    val innerCtx = ctx.addMode(InnerScope)
    op(innerCtx)
    innerCtx.initialContext.enterLatentDefs(innerCtx.owner)
  }


  /** an aggregate of `inInnerScopeContext` within `inIndexStatsContext` */
  def inIndexScopedStatsContext(op: Context => Unit)(implicit ctx: Context): Unit = {
    inIndexStatsContext(inInnerScopeContext(op)(_))(ctx)
  }

  /**Forces lazy annotations, if one is `scala.annotation.internal.Child` then it will add the referenced type as a
   * sealed child.
   */
  private def analyseAnnotations(sym: Symbol)(implicit ctx: Context): Unit = {
    for (annot <- sym.annotations) {
      annot.completeInfo()
      if (annot.tpe.typeSymbolDirect === defn.ChildAnnot) {
        val child = annot.tpe.typeArgs.head.typeSymbolDirect
        sym.addChild(child)
        ctx.log(s"adding sealed child ${showSym(child)} to ${showSym(sym)}")
      }
    }
  }

  /**Maintains state through traversal of a TASTy file, such as the outer scope of the defintion being traversed, the
   * traversal mode, and the root owners and source path for the TASTy file.
   * It also provides all operations for manipulation of the symbol table, such as creating/updating symbols and
   * updating their types.
   */
  sealed abstract class Context { thisCtx =>

    protected implicit final def implyThisCtx: thisCtx.type = thisCtx

    /**Associates the annotations with the symbol, and will force their evaluation if not reading statements.*/
    def adjustAnnotations(sym: Symbol, annots: List[DeferredAnnotation]): Unit = {
      if (annots.nonEmpty) {
        if (mode.is(IndexStats)) {
          log(s"lazily adding annotations to ${showSym(sym)}")
          initialContext.stageSymbolToForceAnnots(sym.setAnnotations(annots.map(_.lzy(sym))))
        }
        else {
          log(s"eagerly adding annotations to ${showSym(sym)}")
          analyseAnnotations(sym.setAnnotations(annots.map(_.eager(sym))))
        }
      }
    }

    final def globallyVisibleOwner: Symbol = owner.logicallyEnclosingMember

    final def ignoreAnnotations: Boolean = u.settings.YtastyNoAnnotations

    def requiresLatentEntry(decl: Symbol): Boolean = decl.isScala3Inline

    def canEnterOverload(decl: Symbol): Boolean = {
      !(decl.isModule && isSymbol(findObject(thisCtx.owner, decl.name)))
    }

    final def log(str: => String): Unit = {
      if (u.settings.YdebugTasty)
        u.reporter.echo(
          pos = u.NoPosition,
          msg = str.linesIterator.map(line => s"#[$classRoot]: $line").mkString(System.lineSeparator)
        )
    }

    def owner: Symbol
    def source: AbstractFile
    def mode: TastyMode

    private final def loadingMirror: u.Mirror = u.mirrorThatLoaded(owner)

    final def requiredPackage(fullname: TastyName): Symbol = fullname match {
      case TastyName.Root | TastyName.RootPkg => loadingMirror.RootPackage
      case TastyName.EmptyPkg                 => loadingMirror.EmptyPackage
      case fullname                           =>
        symOrDependencyError(false, true, fullname)(loadingMirror.getPackage(encodeTermName(fullname).toString))
    }

    private def symOrDependencyError(isObject: Boolean, isPackage: Boolean, fullname: TastyName)(sym: => Symbol): Symbol = {
      try sym
      catch {
        case _: MissingRequirementError =>
          val kind = if (isObject) "object" else if (isPackage) "package" else "class"
          val addendum = if (mode.is(ReadAnnotation)) s" whilst reading annotation of $owner" else ""
          val msg =
            s"could not find $kind ${fullname.source}$addendum; perhaps it is missing from the classpath."
          typeError(msg)
      }
    }

    final lazy val classRoot: Symbol = initialContext.topLevelClass

    final def newLocalDummy: Symbol = owner.newLocalDummy(u.NoPosition)

    final def newWildcard(info: Type): Symbol =
      owner.newTypeParameter(
        name     = u.freshTypeName("_$")(u.currentFreshNameCreator),
        pos      = u.NoPosition,
        newFlags = FlagSets.Creation.Default
      ).setInfo(info)

    final def newConstructor(owner: Symbol, info: Type): Symbol = unsafeNewSymbol(
      owner = owner,
      name  = TastyName.Constructor,
      flags = Method,
      info  = info
    )

    final def findRootSymbol(roots: Set[Symbol], name: TastyName): Option[Symbol] = {
      import TastyName.TypeName

      def isSameRoot(root: Symbol, selector: u.Name): Boolean =
        (root.owner `eq` this.owner) && selector === root.name

      val selector = encodeTastyName(name)
      roots.find(isSameRoot(_,selector)).map(found =>
        name match {
          case TypeName(_: ObjectName) => found.linkedClassOfClass
          case _                       => found
        }
      )
    }

    final def findOuterClassTypeParameter(name: TastyName.TypeName): Symbol = {
      val selector: u.Name = encodeTypeName(name)
      owner.owner.typeParams.find(selector === _.name).getOrElse {
        throw new AssertionError(s"${owner.owner} has no type params.")
      }
    }

    final def newRefinementSymbol(parent: Type, owner: Symbol, name: TastyName, tpe: Type): Symbol = {
      val overridden = parent.member(encodeTastyName(name))
      val isOverride = isSymbol(overridden)
      var flags = EmptyTastyFlags
      if (isOverride && overridden.isType) flags |= Override
      val info = {
        if (name.isTermName) {
          flags |= Method | Deferred
          tpe match {
            case u.TypeRef(_, u.definitions.ByNameParamClass, arg :: Nil) => // nullary method
              u.NullaryMethodType(arg)
            case u.PolyType(tparams, res) if res.paramss.isEmpty => u.PolyType(tparams, u.NullaryMethodType(res))
            case _:u.MethodType | _:u.PolyType => tpe
            case _ => // val, which is not stable if structural. Dotty does not support vars
              if (isOverride && overridden.is(Stable)) flags |= Stable
              u.NullaryMethodType(tpe)
          }
        }
        else {
          if (tpe.isInstanceOf[u.TypeBounds]) flags |= Deferred
          tpe
        }
      }
      unsafeNewSymbol(owner, name, flags, info)
    }

    /** Guards the creation of an object val by checking for an existing definition in the owner's scope
      */
    final def delayCompletion(owner: Symbol, name: TastyName, completer: TastyCompleter, privateWithin: Symbol = noSymbol): Symbol = {
      def default() = unsafeNewSymbol(owner, name, completer.originalFlagSet, completer, privateWithin)
      if (completer.originalFlagSet.is(Object)) {
        val sourceObject = findObject(owner, encodeTermName(name))
        if (isSymbol(sourceObject))
          redefineSymbol(sourceObject, completer.originalFlagSet, completer, privateWithin)
        else
          default()
      }
      else {
        default()
      }
    }

    /** Guards the creation of an object class by checking for an existing definition in the owner's scope
      */
    final def delayClassCompletion(owner: Symbol, typeName: TastyName.TypeName, completer: TastyCompleter, privateWithin: Symbol): Symbol = {
      def default() = unsafeNewClassSymbol(owner, typeName, completer.originalFlagSet, completer, privateWithin)
      if (completer.originalFlagSet.is(Object)) {
        val sourceObject = findObject(owner, encodeTermName(typeName.toTermName))
        if (isSymbol(sourceObject))
          redefineSymbol(sourceObject.objectImplementation, completer.originalFlagSet, completer, privateWithin)
        else
          default()
      }
      else {
        default()
      }
    }

    def evict(sym: Symbol): Unit = {
      sym.owner.rawInfo.decls.unlink(sym)
      sym.info = u.NoType
    }

    final def enterIfUnseen(sym: Symbol): Unit = {
      val decl = declaringSymbolOf(sym)
      if (mode.is(IndexScopedStats))
        initialContext.collectLatentEvidence(owner, decl)
      if (!requiresLatentEntry(decl))
        enterIfUnseen0(owner.rawInfo.decls, decl)
    }

    protected final def enterIfUnseen0(decls: u.Scope, decl: Symbol): Unit = {
      if (allowsOverload(decl) || decl.isParamGetter) {
        if (canEnterOverload(decl)) {
          decls.enter(decl)
        }
      }
      else {
        decls.enterIfNew(decl)
      }
    }

    /** Unsafe to call for creation of a object val, prefer `delayCompletion` if info is a LazyType
      */
    private def unsafeNewSymbol(owner: Symbol, name: TastyName, flags: TastyFlagSet, info: Type, privateWithin: Symbol = noSymbol): Symbol =
      unsafeSetInfoAndPrivate(unsafeNewUntypedSymbol(owner, name, flags), info, privateWithin)

    /** Unsafe to call for creation of a object class, prefer `delayClassCompletion` if info is a LazyType
      */
    private def unsafeNewClassSymbol(owner: Symbol, typeName: TastyName.TypeName, flags: TastyFlagSet, info: Type, privateWithin: Symbol): Symbol =
      unsafeSetInfoAndPrivate(unsafeNewUntypedClassSymbol(owner, typeName, flags), info, privateWithin)

    private final def unsafeNewUntypedSymbol(owner: Symbol, name: TastyName, flags: TastyFlagSet): Symbol = {
      if (flags.isOneOf(Param | ParamSetter)) {
        if (name.isTypeName) {
          owner.newTypeParameter(encodeTypeName(name.toTypeName), u.NoPosition, newSymbolFlagSet(flags))
        }
        else {
          if (owner.isClass && flags.is(FlagSets.FieldGetter)) {
            val fieldFlags = flags &~ FlagSets.FieldGetter | FlagSets.LocalField
            val termName   = encodeTermName(name)
            val getter     = owner.newMethodSymbol(termName, u.NoPosition, newSymbolFlagSet(flags))
            val fieldSym   = owner.newValue(termName, u.NoPosition, newSymbolFlagSet(fieldFlags))
            fieldSym.info  = defn.CopyInfo(getter, fieldFlags)
            owner.rawInfo.decls.enter(fieldSym)
            getter
          }
          else {
            owner.newValueParameter(encodeTermName(name), u.NoPosition, newSymbolFlagSet(flags))
          }
        }
      }
      else if (flags.is(FlagSets.Creation.ObjectDef)) {
        log(s"!!! visited module value $name first")
        val module = owner.newModule(encodeTermName(name), u.NoPosition, newSymbolFlagSet(flags))
        module.moduleClass.info = defn.DefaultInfo
        module
      }
      else if (name.isTypeName) {
        owner.newTypeSymbol(encodeTypeName(name.toTypeName), u.NoPosition, newSymbolFlagSet(flags))
      }
      else if (name === TastyName.Constructor) {
        owner.newConstructor(u.NoPosition, newSymbolFlagSet(flags &~ Stable))
      }
      else if (name === TastyName.MixinConstructor) {
        owner.newMethodSymbol(u.nme.MIXIN_CONSTRUCTOR, u.NoPosition, newSymbolFlagSet(flags &~ Stable))
      }
      else {
        owner.newMethodSymbol(encodeTermName(name), u.NoPosition, newSymbolFlagSet(flags))
      }
    }

    private final def unsafeNewUntypedClassSymbol(owner: Symbol, typeName: TastyName.TypeName, flags: TastyFlagSet): Symbol = {
      if (flags.is(FlagSets.Creation.ObjectClassDef)) {
        log(s"!!! visited module class $typeName first")
        val module = owner.newModule(encodeTermName(typeName), u.NoPosition, FlagSets.Creation.Default)
        module.info = defn.DefaultInfo
        module.moduleClass.flags = newSymbolFlagSet(flags)
        module.moduleClass
      }
      else {
        owner.newClassSymbol(encodeTypeName(typeName), u.NoPosition, newSymbolFlagSet(flags))
      }
    }

    final def enterClassCompletion(): Symbol = {
      val cls = globallyVisibleOwner.asClass
      val assumedSelfType =
        if (cls.is(Object) && cls.owner.isClass) defn.SingleType(cls.owner.thisType, cls.sourceModule)
        else u.NoType
      cls.info = u.ClassInfoType(cls.repr.parents, cls.repr.decls, assumedSelfType.typeSymbolDirect)
      cls
    }

    /** Normalises the parents and sets up value class machinery */
    final def adjustParents(cls: Symbol, parents: List[Type]): List[Type] = {
      val parentTypes = parents.map { tp =>
        val tpe = tp.dealias
        if (tpe.typeSymbolDirect === u.definitions.ObjectClass) u.definitions.AnyRefTpe
        else tpe
      }
      if (parentTypes.head.typeSymbolDirect === u.definitions.AnyValClass) {
        // TODO [tasty]: please reconsider if there is some shared optimised logic that can be triggered instead.
        withPhaseNoLater("extmethods") { ctx0 =>
          // duplicated from scala.tools.nsc.transform.ExtensionMethods
          cls.primaryConstructor.makeNotPrivate(noSymbol)
          for (decl <- cls.info.decls if decl.isMethod) {
            if (decl.isParamAccessor) decl.makeNotPrivate(cls)
            if (!decl.isClassConstructor) {
              val extensionMeth = decl.newExtensionMethodSymbol(cls.companion, u.NoPosition)
              extensionMeth setInfo u.extensionMethInfo(cls, extensionMeth, decl.info, cls)
            }
          }
        }
      }
      parentTypes
    }

    private[bridge] final def resetFlag0(symbol: Symbol, flags: u.FlagSet): symbol.type =
      symbol.resetFlag(flags)

    final def completeEnumSingleton(sym: Symbol, tpe: Type): Unit = {
      val moduleCls = sym.moduleClass
      val moduleClsFlags = FlagSets.withAccess(
        flags = FlagSets.Creation.ObjectClassDef,
        inheritedAccess = sym.repr.originalFlagSet
      )
      val selfTpe = defn.SingleType(sym.owner.thisPrefix, sym)
      val ctor = newConstructor(moduleCls, selfTpe)
      moduleCls.typeOfThis = selfTpe
      moduleCls.flags = newSymbolFlagSet(moduleClsFlags)
      moduleCls.info = defn.ClassInfoType(intersectionParts(tpe), ctor :: Nil, moduleCls)
      moduleCls.privateWithin = sym.privateWithin
    }

    final def redefineSymbol(symbol: Symbol, flags: TastyFlagSet, completer: TastyCompleter, privateWithin: Symbol): symbol.type = {
      symbol.flags = newSymbolFlagSet(flags)
      unsafeSetInfoAndPrivate(symbol, completer, privateWithin)
    }

    private def unsafeSetInfoAndPrivate(symbol: Symbol, info: Type, privateWithin: Symbol): symbol.type = {
      symbol.privateWithin = privateWithin
      symbol.info = info
      symbol
    }

    /** Determines the owner of a refinement in the current context by the following steps:
     *  1) if the owner if this context is a refinement symbol, we are in a recursive RefinedType. Ensure that the
     *     context owner is initialised with the parent and reuse it.
     *  2) if the parent is also a RefinedType, then we will flatten the nested structure by reusing its owner
     *  3) the parent is not a RefinedType, and we are not in an enclosing RefinedType, so create a new RefinementClassSymbol.
     *  The Parent alongside the RefinedType owner are passed to the given operation
     */
    final def enterRefinement[T](parent: Type)(op: Context => T): T = {
      val clazz = owner match {
        case enclosing: u.RefinementClassSymbol =>
          if (!enclosing.hasRawInfo) mkRefinedTypeWith(parent :: Nil, enclosing, u.newScope)
          enclosing
        case _ => parent match {
          case nested: u.RefinedType => nested.typeSymbol
          case _                     => newRefinementClassSymbol
        }
      }
      op(withOwner(clazz))
    }

    final def newRefinementClassSymbol: Symbol = owner.newRefinementClass(u.NoPosition)

    final def setInfo(sym: Symbol, info: Type): Unit = sym.info = info

    final def markAsEnumSingleton(sym: Symbol): Unit =
      sym.updateAttachment(u.DottyEnumSingleton)

    final def markAsOpaqueType(sym: Symbol, alias: Type): Unit =
      sym.updateAttachment(new u.DottyOpaqueTypeAlias(alias))

    final def onCompletionError[T](sym: Symbol): PartialFunction[Throwable, T] = {
      case err: u.TypeError =>
        sym.info = u.ErrorType
        throw err
    }

    @tailrec
    final def initialContext: InitialContext = this match {
      case ctx: InitialContext => ctx
      case ctx: FreshContext   => ctx.outer.initialContext
    }

    final def withOwner(owner: Symbol): Context =
      if (owner `ne` this.owner) freshSymbol(owner) else this

    final def withNewScope: Context =
      freshSymbol(newLocalDummy)

    final def freshSymbol(owner: Symbol): FreshContext = new FreshContext(owner, this, this.mode)
    final def freshMode(mode: TastyMode): FreshContext = new FreshContext(this.owner, this, mode)
    final def fresh: FreshContext                      = new FreshContext(this.owner, this, this.mode)

    final def addMode(mode: TastyMode): Context =
      if (!this.mode.is(mode)) freshMode(this.mode | mode)
      else this

    final def retractMode(mode: TastyMode): Context =
      if (this.mode.isOneOf(mode)) freshMode(this.mode &~ mode)
      else this

    final def withMode(mode: TastyMode): Context =
      if (mode != this.mode) freshMode(mode)
      else this

    final def withSource(source: AbstractFile): Context =
      if (source `ne` this.source) fresh.atSource(source)
      else this

    final def withPhaseNoLater[T](phase: String)(op: Context => T): T =
      u.enteringPhaseNotLaterThan[T](u.findPhaseWithName(phase))(op(this))
  }


  final class InitialContext(val topLevelClass: Symbol, val source: AbstractFile) extends Context {
    def mode: TastyMode = EmptyTastyMode
    def owner: Symbol = topLevelClass.owner

    private[this] var mySymbolsToForceAnnots: mutable.LinkedHashSet[Symbol] = _

    private[ContextOps] def stageSymbolToForceAnnots(sym: Symbol): Unit = {
      if (sym.annotations.nonEmpty) {
        if (mySymbolsToForceAnnots == null) {
          mySymbolsToForceAnnots = mutable.LinkedHashSet.empty
        }
        mySymbolsToForceAnnots += sym
      }
    }

    /** Force any lazy annotations collected from declaration statements directly in this scope.
     *
     *  It is important to call this *after* indexing statements in a scope, otherwise calling
     *  `ownertree.findOwner` can fail, this is because `ownertree.findOwner` cannot traverse a definition tree at
     *  a given address before a symbol has been registered to that address.
     */
    private[ContextOps] def forceAnnotations(): Unit = {
      if (mySymbolsToForceAnnots != null) {
        val toForce = mySymbolsToForceAnnots.toList
        mySymbolsToForceAnnots.clear()
        for (sym <- toForce) {
          log(s"!!! forcing annotations on ${showSym(sym)}")
          analyseAnnotations(sym)
        }
        assert(mySymbolsToForceAnnots.isEmpty, "more symbols added while forcing")
      }
    }

    private[this] var myInlineDefs: mutable.Map[Symbol, mutable.ArrayBuffer[Symbol]] = null
    private[this] var myMacros: mutable.Map[Symbol, mutable.ArrayBuffer[Symbol]] = null
    private[this] var myTraitParamAccessors: mutable.Map[Symbol, mutable.ArrayBuffer[Symbol]] = null

    /** Collect evidence from definitions that is required by `enterLatentDefs`. */
    private[ContextOps] def collectLatentEvidence(owner: Symbol, sym: Symbol): Unit = {

      def macroMap() = {
        if (myMacros == null) myMacros = mutable.HashMap.empty
        myMacros
      }

      def inlineMap() = {
        if (myInlineDefs == null) myInlineDefs = mutable.HashMap.empty
        myInlineDefs
      }

      def traitParamAccessors() = {
        if (myTraitParamAccessors == null) myTraitParamAccessors = mutable.HashMap.empty
        myTraitParamAccessors
      }

      def append(map: mutable.Map[Symbol, mutable.ArrayBuffer[Symbol]])(owner: Symbol, sym: Symbol) =
        map.getOrElseUpdate(owner, mutable.ArrayBuffer.empty) += sym

      if (sym.isScala2Macro) append(macroMap())(owner, sym)
      else if (sym.isScala3Inline) append(inlineMap())(owner, sym)
      else if (sym.isTraitParamAccessor) append(traitParamAccessors())(owner, sym)

    }

    /**Should be called after indexing all symbols in the given owners scope.
     *
     * Enters qualifying definitions into the given owners scope, according to the following rules:
     *   - an `inline macro` method (Scala 3 macro) without a corresponding `erased macro` method (Scala 2 macro).
     *
     * Reports illegal definitions:
     *   - trait constructors with parameters
     *
     *  @param cls should be a symbol associated with a non-empty scope
     */
    private[ContextOps] def enterLatentDefs(cls: Symbol): Unit = {

      def macroDefs(cls: Symbol): Option[Iterable[Symbol]] = {
        if (myMacros != null) myMacros.remove(cls)
        else None
      }

      def inlineDefs(cls: Symbol): Option[Iterable[Symbol]] = {
        if (myInlineDefs != null) myInlineDefs.remove(cls)
        else None
      }

      def traitParamAccessors(cls: Symbol): Option[Iterable[Symbol]] = {
        if (myTraitParamAccessors != null) myTraitParamAccessors.remove(cls)
        else None
      }

      def enterInlineDefs(cls: Symbol, decls: u.Scope): Unit = {
        val macros = macroDefs(cls).getOrElse(Iterable.empty)
        val defs   = inlineDefs(cls).getOrElse(Iterable.empty)

        for (d <- defs if !macros.exists(_.name == d.name))
          enterIfUnseen0(decls, d)
      }

      def reportParameterizedTrait(cls: Symbol, decls: u.Scope): Unit = {
        val traitParams = traitParamAccessors(cls).getOrElse(Iterable.empty)
        if (traitParams.nonEmpty) {
          log {
            val parameters = traitParams.map(_.nameString)
            s"parameterized trait ${parameters.mkString(s"${cls.nameString}(", ", ", ")")}"
          }
          cls.updateAttachment(new u.DottyParameterisedTrait(traitParams.toList))
        }
      }

      val decls = cls.rawInfo.decls
      enterInlineDefs(cls, decls)
      reportParameterizedTrait(cls, decls)

    }
  }

  final class FreshContext(val owner: Symbol, val outer: Context, val mode: TastyMode) extends Context {
    private[this] var mySource: AbstractFile = null
    def atSource(source: AbstractFile): this.type = { mySource = source ; this }
    def source: AbstractFile = if (mySource == null) outer.source else mySource
  }
}
