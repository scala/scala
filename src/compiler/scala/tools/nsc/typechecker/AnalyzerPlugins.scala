/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package typechecker

import scala.annotation._

/**
 *  @author Lukas Rytz
 */
trait AnalyzerPlugins { self: Analyzer with splain.SplainData =>
  import global._

  @nowarn
  trait AnalyzerPlugin {
    /**
     * Selectively activate this analyzer plugin, e.g. according to the compiler phase.
     *
     * Note that the current phase can differ from the global compiler phase (look for `enteringPhase`
     * invocations in the compiler). For instance, lazy types created by the UnPickler are completed
     * at the phase in which their symbol is created. Observations show that this can even be the
     * parser phase. Since symbol completion can trigger subtyping, typing etc, your plugin might
     * need to be active also in phases other than namer and typer.
     *
     * Typically, this method can be implemented as
     *
     *   global.phase.id < global.currentRun.picklerPhase.id
     */
    def isActive(): Boolean = true

    /**
     * Let analyzer plugins change the expected type before type checking a tree.
     */
    def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = pt

    /**
     * Let analyzer plugins modify the type that has been computed for a tree.
     *
     * @param tpe   The type inferred by the type checker, initially (for first plugin) `tree.tpe`
     * @param typer The typer that type checked `tree`
     * @param tree  The type-checked tree
     * @param mode  Mode that was used for typing `tree`
     * @param pt    Expected type that was used for typing `tree`
     */
    def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = tpe

    /**
     * Let analyzer plugins change the types assigned to definitions. For definitions that have
     * an annotated type, the assigned type is obtained by typing that type tree. Otherwise, the
     * type is inferred by typing the definition's righthand side, or from the overridden
     * member under `-Xsource-features`.
     *
     * In order to know if the type was inferred, you can query the `wasEmpty` field in the `tpt`
     * TypeTree of the definition (for DefDef and ValDef).
     *
     * (*) If the type of a method or value is inferred, the type-checked tree is stored in the
     * `analyzer.transformed` hash map, indexed by the definition's rhs tree.
     *
     * NOTE: Invoking the type checker can lead to cyclic reference errors. For instance, if this
     * method is called from the type completer of a recursive method, type checking the method
     * rhs will invoke the same completer again. It might be possible to avoid this situation by
     * assigning `tpe` to `defTree.symbol` (untested) - the final type computed by this method
     * will then be assigned to the definition's symbol by monoTypeCompleter (in Namers).
     *
     * The hooks into `typeSig` allow analyzer plugins to add annotations to (or change the types
     * of) definition symbols. This cannot not be achieved by using `pluginsTyped`: this method
     * is only called during type checking, so changing the type of a symbol at this point is too
     * late: references to the symbol might already be typed and therefore obtain the original
     * type assigned during naming.
     *
     * @param defTree is the definition for which the type was computed. The different cases are
     * outlined below. Note that this type is untyped (for methods and values with inferred type,
     * the typed rhs trees are available in analyzer.transformed).
     *
     * Case defTree: Template
     *   - tpe  : A ClassInfoType for the template
     *   - typer: The typer for template members, i.e. expressions and definitions of defTree.body
     *   - pt   : WildcardType
     *   - the class symbol is accessible through typer.context.owner
     *
     * Case defTree: ClassDef
     *   - tpe  : A ClassInfoType, or a PolyType(params, ClassInfoType) for polymorphic classes.
     *            The class type is the one computed by templateSig, i.e. through the above case
     *   - typer: The typer for the class. Note that this typer has a different context than the
     *            typer for the template.
     *   - pt   : WildcardType
     *
     * Case defTree: ModuleDef
     *   - tpe  : A ClassInfoType computed by templateSig
     *   - typer: The typer for the module. context.owner of this typer is the module class symbol
     *   - pt   : WildcardType
     *
     * Case defTree: DefDef
     *   - tpe  : The type of the method (MethodType, PolyType or NullaryMethodType). (*)
     *   - typer: The typer the rhs of this method
     *   - pt   : If tpt.isEmpty, either the result type from the overridden method, or WildcardType.
     *            Otherwise the type obtained from typing tpt.
     *   - Note that for constructors, pt is the class type which the constructor creates. To type
     *     check the rhs of the constructor however, the expected type has to be WildcardType (see
     *     Typers.typedDefDef)
     *
     * Case defTree: ValDef
     *   - tpe  : The type of this value. (*)
     *   - typer: The typer for the rhs of this value
     *   - pt   : If tpt.isEmpty, WildcardType. Otherwise the type obtained from typing tpt.
     *   - Note that pluginsTypeSig might be called multiple times for the same ValDef since it is
     *     used to compute the types of the accessor methods (see `pluginsTypeSigAccessor`)
     *
     * Case defTree: TypeDef
     *   - tpe  : The type obtained from typing rhs (PolyType if the TypeDef defines a polymorphic type)
     *   - typer: The typer for the rhs of this type
     *   - pt   : WildcardType
     */
    def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = tpe

    /**
     * Modify the types of field accessors. The namer phase creates method types for getters and
     * setters based on the type of the corresponding field.
     *
     * Note: in order to compute the method type of an accessor, the namer calls `typeSig` on the
     * `ValDef` tree of the corresponding field. This implies that the `pluginsTypeSig` method
     * is potentially called multiple times for the same ValDef tree.
     *
     * @param tpe   The method type created by the namer for the accessor
     * @param typer The typer for the ValDef (not for the rhs)
     * @param tree  The ValDef corresponding to the accessor
     * @param sym   The accessor method symbol (getter, setter, beanGetter or beanSetter)
     */
    def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type = tpe

    /**
     * Decide whether this analyzer plugin can adapt a tree that has an annotated type to the
     * given type tp, taking into account the given mode (see method adapt in trait Typers).
     */
    def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = false

    /**
     * Adapt a tree that has an annotated type to the given type tp, taking into account the given
     * mode (see method adapt in trait Typers).
     *
     * An implementation cannot rely on canAdaptAnnotations being called before. If the implementing
     * class cannot do the adapting, it should return the tree unchanged.
     */
    def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = tree

    /**
     * Modify the type of a return expression. By default, return expressions have type
     * NothingTpe.
     *
     * @param tpe   The type of the return expression
     * @param typer The typer that was used for typing the return tree
     * @param tree  The typed return expression tree
     * @param pt    The return type of the enclosing method
     */
    def pluginsTypedReturn(tpe: Type, typer: Typer, tree: Return, pt: Type): Type = tpe

    /**
     * Access the search instance that will be used for the implicit search.
     *
     * The motivation of this method is to allow analyzer plugins to control when/where
     * implicit search are triggered, and access their environment for data capturing purposes.
     *
     * @param search The instance that holds all the information about a given implicit search.
     */
    def pluginsNotifyImplicitSearch(search: ImplicitSearch): Unit = ()

    /**
     * Access the implicit search result from Scalac's typechecker.
     *
     * The motivation of this method is to allow analyzer plugins to control when/where
     * implicit search results are returned, and inspect them for data capturing purposes.
     *
     * @param result The result to a given implicit search.
     */
    def pluginsNotifyImplicitSearchResult(result: SearchResult): Unit = ()

    /**
     * Construct a custom error message for implicit parameters that could not be resolved.
     *
     * @param param The implicit parameter that was resolved
     * @param errors The chain of intermediate implicits that lead to this error
     * @param previous The error message constructed by the previous analyzer plugin, or the builtin default
     */
    def noImplicitFoundError(param: Symbol, errors: List[ImplicitError], previous: String): String =
      previous
  }

  /**
   * @define nonCumulativeReturnValueDoc Returns `None` if the plugin doesn't want to customize the default behavior
   * or something else if the plugin knows better that the implementation provided in scala-compiler.jar.
   * If multiple plugins return a non-empty result, it's going to be a compilation error.
   */
  @nowarn
  trait MacroPlugin {
    /**
     * Selectively activate this analyzer plugin, e.g. according to the compiler phase.
     *
     * Note that the current phase can differ from the global compiler phase (look for `enteringPhase`
     * invocations in the compiler). For instance, lazy types created by the UnPickler are completed
     * at the phase in which their symbol is created. Observations show that this can even be the
     * parser phase. Since symbol completion can trigger subtyping, typing etc, your plugin might
     * need to be active also in phases other than namer and typer.
     *
     * Typically, this method can be implemented as
     *
     *   global.phase.id < global.currentRun.picklerPhase.id
     */
    def isActive(): Boolean = true

    /**
     * Typechecks the right-hand side of a macro definition (which typically features
     * a mere reference to a macro implementation).
     *
     * Default implementation provided in `self.standardTypedMacroBody` makes sure that the rhs
     * resolves to a reference to a method in either a static object or a macro bundle,
     * verifies that the referred method is compatible with the macro def and upon success
     * attaches a macro impl binding to the macro def's symbol.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Option[Tree] = None

    /**
     * Figures out whether the given macro definition is blackbox or whitebox.
     *
     * Default implementation provided in `self.standardIsBlackbox` loads the macro impl binding
     * and fetches boxity from the "isBlackbox" field of the macro signature.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsIsBlackbox(macroDef: Symbol): Option[Boolean] = None

    /**
     * Expands an application of a def macro (i.e. of a symbol that has the MACRO flag set),
     * possibly using the current typer mode and the provided prototype.
     *
     * Default implementation provided in `self.standardMacroExpand` figures out whether the `expandee`
     * needs to be expanded right away or its expansion has to be delayed until all undetermined
     * parameters are inferred, then loads the macro implementation using `self.pluginsMacroRuntime`,
     * prepares the invocation arguments for the macro implementation using `self.pluginsMacroArgs`,
     * and finally calls into the macro implementation. After the call returns, it typechecks
     * the expansion and performs some bookkeeping.
     *
     * This method is typically implemented if your plugin requires significant changes to the macro engine.
     * If you only need to customize the macro context, consider implementing `pluginsMacroArgs`.
     * If you only need to customize how macro implementation are invoked, consider going for `pluginsMacroRuntime`.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Option[Tree] = None

    /**
     * Computes the arguments that need to be passed to the macro impl corresponding to a particular expandee.
     *
     * Default implementation provided in `self.standardMacroArgs` instantiates a `scala.reflect.macros.contexts.Context`,
     * gathers type and value arguments of the macro application and throws them together into `MacroArgs`.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsMacroArgs(typer: Typer, expandee: Tree): Option[MacroArgs] = None

    /**
     * Summons a function that encapsulates macro implementation invocations for a particular expandee.
     *
     * Default implementation provided in `self.standardMacroRuntime` returns a function that
     * loads the macro implementation binding from the macro definition symbol,
     * then uses either Java or Scala reflection to acquire the method that corresponds to the impl,
     * and then reflectively calls into that method.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsMacroRuntime(expandee: Tree): Option[MacroRuntime] = None

    /**
     * Creates a symbol for the given tree in lexical context encapsulated by the given namer.
     *
     * Default implementation provided in `namer.standardEnterSym` handles MemberDef's and Imports,
     * doing nothing for other trees (DocDef's are seen through and rewrapped). Typical implementation
     * of `enterSym` for a particular tree flavor creates a corresponding symbol, assigns it to the tree,
     * enters the symbol into scope and then might even perform some code generation.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsEnterSym(namer: Namer, tree: Tree): Boolean = false

    /**
     * Makes sure that for the given class definition, there exists a companion object definition.
     *
     * Default implementation provided in `namer.standardEnsureCompanionObject` looks up a companion symbol for the class definition
     * and then checks whether the resulting symbol exists or not. If it exists, then nothing else is done.
     * If not, a synthetic object definition is created using the provided factory, which is then entered into namer's scope.
     *
     * $nonCumulativeReturnValueDoc.
     */
    def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Option[Symbol] = None

    /**
     * Prepares a list of statements for being typechecked by performing domain-specific type-agnostic code synthesis.
     *
     * Trees passed into this method are going to be named, but not typed.
     * In particular, you can rely on the compiler having called `enterSym` on every stat prior to passing calling this method.
     *
     * Default implementation does nothing. Current approaches to code syntheses (generation of underlying fields
     * for getters/setters, creation of companion objects for case classes, etc) are too disparate and ad-hoc
     * to be treated uniformly, so I'm leaving this for future work.
     */
    def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = stats
  }



  /** A list of registered analyzer plugins */
  private var analyzerPlugins: List[AnalyzerPlugin] = Nil

  /** Registers a new analyzer plugin */
  def addAnalyzerPlugin(plugin: AnalyzerPlugin): Unit = {
    if (!analyzerPlugins.contains(plugin))
      analyzerPlugins = plugin :: analyzerPlugins
  }

  private abstract class CumulativeOp[T] {
    def default: T
    def accumulate: (T, AnalyzerPlugin) => T
  }

  private def invoke[T](op: CumulativeOp[T]): T = {
    if (analyzerPlugins.isEmpty) op.default
    else analyzerPlugins.foldLeft(op.default)((current, plugin) =>
      if (!plugin.isActive()) current else op.accumulate(current, plugin))
  }

  /** @see AnalyzerPlugin.pluginsPt */
  def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Mode): Type = {
    var result = pt
    var plugins = analyzerPlugins
    while (!plugins.isEmpty) { // OPT use loop rather than the invoke combinator to reduce allocations
      result = plugins.head.pluginsPt(result, typer, tree, mode)
      plugins = plugins.tail
    }
    result
  }

  /** @see AnalyzerPlugin.pluginsTyped */
  def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Mode, pt: Type): Type = {
    var result = addAnnotations(tree, tpe)
    var plugins = analyzerPlugins
    while (!plugins.isEmpty) { // OPT use loop rather than the invoke combinator to reduce allocations
      result = plugins.head.pluginsTyped(result, typer, tree, mode, pt)
      plugins = plugins.tail
    }
    result
  }

  /** @see AnalyzerPlugin.pluginsTypeSig */
  def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = invoke(new CumulativeOp[Type] {
    def default = tpe
    def accumulate = (tpe, p) => p.pluginsTypeSig(tpe, typer, defTree, pt)
  })

  /** @see AnalyzerPlugin.pluginsTypeSigAccessor */
  def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type = invoke(new CumulativeOp[Type] {
    def default = tpe
    def accumulate = (tpe, p) => p.pluginsTypeSigAccessor(tpe, typer, tree, sym)
  })

  /** @see AnalyzerPlugin.canAdaptAnnotations */
  def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Boolean = invoke(new CumulativeOp[Boolean] {
    // support deprecated methods in annotation checkers
    def default = global.canAdaptAnnotations(tree, mode, pt)
    def accumulate = (curr, p) => curr || p.canAdaptAnnotations(tree, typer, mode, pt)
  })

  /** @see AnalyzerPlugin.adaptAnnotations */
  def adaptAnnotations(tree: Tree, typer: Typer, mode: Mode, pt: Type): Tree = invoke(new CumulativeOp[Tree] {
    // support deprecated methods in annotation checkers
    def default = global.adaptAnnotations(tree, mode, pt)
    def accumulate = (tree, p) => p.adaptAnnotations(tree, typer, mode, pt)
  })

  /** @see AnalyzerPlugin.pluginsTypedReturn */
  def pluginsTypedReturn(tpe: Type, typer: Typer, tree: Return, pt: Type): Type = invoke(new CumulativeOp[Type] {
    def default = adaptTypeOfReturn(tree.expr, pt, tpe)
    def accumulate = (tpe, p) => p.pluginsTypedReturn(tpe, typer, tree, pt)
  })

  /** @see AnalyzerPlugin.pluginsImplicitSearch */
  def pluginsNotifyImplicitSearch(search: ImplicitSearch): Unit = invoke(new CumulativeOp[Unit] {
    def default = ()
    def accumulate = (_, p) => p.pluginsNotifyImplicitSearch(search)
  })

  /** @see AnalyzerPlugin.pluginsImplicitSearchResult */
  def pluginsNotifyImplicitSearchResult(result: SearchResult): Unit = invoke(new CumulativeOp[Unit] {
    def default = ()
    def accumulate = (_, p) => p.pluginsNotifyImplicitSearchResult(result)
  })

  /** @see AnalyzerPlugin.noImplicitFoundError */
  def pluginsNoImplicitFoundError(param: Symbol, errors: List[ImplicitError], initial: String): String =
    invoke(new CumulativeOp[String] {
      def default = initial
      def accumulate = (previous, p) => p.noImplicitFoundError(param, errors, previous)
    })

  /** A list of registered macro plugins */
  private var macroPlugins: List[MacroPlugin] = Nil

  /** Registers a new macro plugin */
  def addMacroPlugin(plugin: MacroPlugin): Unit = {
    if (!macroPlugins.contains(plugin))
      macroPlugins = plugin :: macroPlugins
  }

  private abstract class NonCumulativeOp[T] {
    def position: Position
    def description: String
    def default: T
    def custom(plugin: MacroPlugin): Option[T]
  }

  private def invoke[T](op: NonCumulativeOp[T]): T = {
    if (macroPlugins.isEmpty) op.default
    else {
      var result: Option[T] = None
      var resultPlugin: MacroPlugin = null
      var plugins = macroPlugins
      while (!plugins.isEmpty) {
        val plugin = plugins.head
        if (plugin.isActive()) {
          op.custom(plugin) match {
            case None =>
            case s @ Some(_) =>
              if (result.isDefined) {
                typer.context.error(op.position, s"both $resultPlugin and $plugin want to ${op.description}")
                op.default
              } else {
                result = s
                resultPlugin = plugin
              }
          }
        }
        plugins = plugins.tail
      }
      result.getOrElse(op.default)
    }
  }

  /** @see MacroPlugin.pluginsTypedMacroBody */
  def pluginsTypedMacroBody(typer: Typer, ddef: DefDef): Tree = invoke(new NonCumulativeOp[Tree] {
    def position = ddef.pos
    def description = "typecheck this macro definition"
    def default = standardTypedMacroBody(typer, ddef)
    def custom(plugin: MacroPlugin) = plugin.pluginsTypedMacroBody(typer, ddef)
  })

  /** @see MacroPlugin.pluginsIsBlackbox */
  def pluginsIsBlackbox(macroDef: Symbol): Boolean = invoke(new NonCumulativeOp[Boolean] {
    def position = macroDef.pos
    def description = "compute boxity for this macro definition"
    def default = standardIsBlackbox(macroDef)
    def custom(plugin: MacroPlugin) = plugin.pluginsIsBlackbox(macroDef)
  })

  /** @see MacroPlugin.pluginsMacroExpand */
  def pluginsMacroExpand(typer: Typer, expandee: Tree, mode: Mode, pt: Type): Tree = invoke(new NonCumulativeOp[Tree] {
    def position = expandee.pos
    def description = "expand this macro application"
    def default = standardMacroExpand(typer, expandee, mode, pt)
    def custom(plugin: MacroPlugin) = plugin.pluginsMacroExpand(typer, expandee, mode, pt)
  })

  /** @see MacroPlugin.pluginsMacroArgs */
  def pluginsMacroArgs(typer: Typer, expandee: Tree): MacroArgs = invoke(new NonCumulativeOp[MacroArgs] {
    def position = expandee.pos
    def description = "compute macro arguments for this macro application"
    def default = standardMacroArgs(typer, expandee)
    def custom(plugin: MacroPlugin) = plugin.pluginsMacroArgs(typer, expandee)
  })

  /** @see MacroPlugin.pluginsMacroRuntime */
  def pluginsMacroRuntime(expandee: Tree): MacroRuntime = invoke(new NonCumulativeOp[MacroRuntime] {
    def position = expandee.pos
    def description = "compute macro runtime for this macro application"
    def default = standardMacroRuntime(expandee)
    def custom(plugin: MacroPlugin) = plugin.pluginsMacroRuntime(expandee)
  })

  /** @see MacroPlugin.pluginsEnterSym */
  def pluginsEnterSym(namer: Namer, tree: Tree): Context =
    if (macroPlugins.isEmpty) namer.standardEnterSym(tree)
    else invoke(new NonCumulativeOp[Context] {
      def position = tree.pos
      def description = "enter a symbol for this tree"
      def default = namer.standardEnterSym(tree)
      def custom(plugin: MacroPlugin) = {
        val hasExistingSym = tree.symbol != NoSymbol
        val result = plugin.pluginsEnterSym(namer, tree)
        if (result && hasExistingSym) Some(namer.context)
        else if (result && tree.isInstanceOf[Import]) Some(namer.context.make(tree))
        else if (result) Some(namer.context)
        else None
      }
    })

  /** @see MacroPlugin.pluginsEnsureCompanionObject */
  def pluginsEnsureCompanionObject(namer: Namer, cdef: ClassDef, creator: ClassDef => Tree = companionModuleDef(_)): Symbol = invoke(new NonCumulativeOp[Symbol] {
    def position = cdef.pos
    def description = "enter a companion symbol for this tree"
    def default = namer.standardEnsureCompanionObject(cdef, creator)
    def custom(plugin: MacroPlugin) = plugin.pluginsEnsureCompanionObject(namer, cdef, creator)
  })

  /** @see MacroPlugin.pluginsEnterStats */
  def pluginsEnterStats(typer: Typer, stats: List[Tree]): List[Tree] = {
    // performance opt
    if (macroPlugins.isEmpty) stats
    else macroPlugins.foldLeft(stats)((current, plugin) =>
      if (!plugin.isActive()) current else plugin.pluginsEnterStats(typer, current))
  }
}
