/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.tools.nsc
package typechecker

/**
 *  @author Lukas Rytz
 *  @version 1.0
 */
trait AnalyzerPlugins { self: Analyzer =>
  import global._


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
    def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Int): Type = pt

    /**
     * Let analyzer plugins modify the type that has been computed for a tree.
     *
     * @param tpe   The type inferred by the type checker, initially (for first plugin) `tree.tpe`
     * @param typer The yper that type checked `tree`
     * @param tree  The type-checked tree
     * @param mode  Mode that was used for typing `tree`
     * @param pt    Expected type that was used for typing `tree`
     */
    def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Int, pt: Type): Type = tpe

    /**
     * Let analyzer plugins change the types assigned to definitions. For definitions that have
     * an annotated type, the assigned type is obtained by typing that type tree. Otherwise, the
     * type is inferred by typing the definition's righthand side.
     *
     * In order to know if the type was inferred, you can query the `wasEmpty` field in the `tpt`
     * TypeTree of the definition (for DefDef and ValDef).
     *
     * (*) If the type of a method or value is inferred, the type-checked tree is stored in the
     * `analyzer.transformed` hash map, indexed by the definition's rhs tree.
     *
     * NOTE: Invoking the type checker can lead to cyclic reference errors. For instance, if this
     * method is called from the type completer of a recursive method, type checking the mehtod
     * rhs will invoke the same completer again. It might be possible to avoid this situation by
     * assigning `tpe` to `defTree.symbol` (untested) - the final type computed by this method
     * will then be assigned to the definition's symbol by monoTypeCompleter (in Namers).
     *
     * The hooks into `typeSig` allow analyzer plugins to add annotations to (or change the types
     * of) definition symbols. This cannot not be achieved by using `pluginsTyped`: this method
     * is only called during type checking, so changing the type of a symbol at this point is too
     * late: references to the symbol might already be typed and therefore obtain the the original
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
    def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Int, pt: Type): Boolean = false

    /**
     * Adapt a tree that has an annotated type to the given type tp, taking into account the given
     * mode (see method adapt in trait Typers).
     *
     * An implementation cannot rely on canAdaptAnnotations being called before. If the implementing
     * class cannot do the adapting, it should return the tree unchanged.
     */
    def adaptAnnotations(tree: Tree, typer: Typer, mode: Int, pt: Type): Tree = tree

    /**
     * Modify the type of a return expression. By default, return expressions have type
     * NothingClass.tpe.
     *
     * @param tpe   The type of the return expression
     * @param typer The typer that was used for typing the return tree
     * @param tree  The typed return expression tree
     * @param pt    The return type of the enclosing method
     */
    def pluginsTypedReturn(tpe: Type, typer: Typer, tree: Return, pt: Type): Type = tpe
  }



  /** A list of registered analyzer plugins */
  private var analyzerPlugins: List[AnalyzerPlugin] = Nil

  /** Registers a new analyzer plugin */
  def addAnalyzerPlugin(plugin: AnalyzerPlugin) {
    if (!analyzerPlugins.contains(plugin))
      analyzerPlugins = plugin :: analyzerPlugins
  }


  /** @see AnalyzerPlugin.pluginsPt */
  def pluginsPt(pt: Type, typer: Typer, tree: Tree, mode: Int): Type =
    if (analyzerPlugins.isEmpty) pt
    else analyzerPlugins.foldLeft(pt)((pt, plugin) =>
      if (!plugin.isActive()) pt else plugin.pluginsPt(pt, typer, tree, mode))

  /** @see AnalyzerPlugin.pluginsTyped */
  def pluginsTyped(tpe: Type, typer: Typer, tree: Tree, mode: Int, pt: Type): Type = {
    // support deprecated methods in annotation checkers
    val annotCheckersTpe = addAnnotations(tree, tpe)
    if (analyzerPlugins.isEmpty) annotCheckersTpe
    else analyzerPlugins.foldLeft(annotCheckersTpe)((tpe, plugin) =>
      if (!plugin.isActive()) tpe else plugin.pluginsTyped(tpe, typer, tree, mode, pt))
  }

  /** @see AnalyzerPlugin.pluginsTypeSig */
  def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type =
    if (analyzerPlugins.isEmpty) tpe
    else analyzerPlugins.foldLeft(tpe)((tpe, plugin) =>
      if (!plugin.isActive()) tpe else plugin.pluginsTypeSig(tpe, typer, defTree, pt))

  /** @see AnalyzerPlugin.pluginsTypeSigAccessor */
  def pluginsTypeSigAccessor(tpe: Type, typer: Typer, tree: ValDef, sym: Symbol): Type =
    if (analyzerPlugins.isEmpty) tpe
    else analyzerPlugins.foldLeft(tpe)((tpe, plugin) =>
      if (!plugin.isActive()) tpe else plugin.pluginsTypeSigAccessor(tpe, typer, tree, sym))

  /** @see AnalyzerPlugin.canAdaptAnnotations */
  def canAdaptAnnotations(tree: Tree, typer: Typer, mode: Int, pt: Type): Boolean = {
    // support deprecated methods in annotation checkers
    val annotCheckersExists = global.canAdaptAnnotations(tree, mode, pt)
    annotCheckersExists || {
      if (analyzerPlugins.isEmpty) false
      else analyzerPlugins.exists(plugin =>
        plugin.isActive() && plugin.canAdaptAnnotations(tree, typer, mode, pt))
    }
  }

  /** @see AnalyzerPlugin.adaptAnnotations */
  def adaptAnnotations(tree: Tree, typer: Typer, mode: Int, pt: Type): Tree = {
    // support deprecated methods in annotation checkers
    val annotCheckersTree = global.adaptAnnotations(tree, mode, pt)
    if (analyzerPlugins.isEmpty) annotCheckersTree
    else analyzerPlugins.foldLeft(annotCheckersTree)((tree, plugin) =>
      if (!plugin.isActive()) tree else plugin.adaptAnnotations(tree, typer, mode, pt))
  }

  /** @see AnalyzerPlugin.pluginsTypedReturn */
  def pluginsTypedReturn(tpe: Type, typer: Typer, tree: Return, pt: Type): Type = {
    val annotCheckersType = adaptTypeOfReturn(tree.expr, pt, tpe)
    if (analyzerPlugins.isEmpty) annotCheckersType
    else analyzerPlugins.foldLeft(annotCheckersType)((tpe, plugin) =>
      if (!plugin.isActive()) tpe else plugin.pluginsTypedReturn(tpe, typer, tree, pt))
  }
}
