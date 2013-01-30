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


  abstract class AnalyzerPlugin {
    /**
     * An annotation checker associated with this analyzer plugin. If this field is non-empty,
     * the annotation checker is automatically registered by global.analyzer.addAnalyzerPlugin.
     */
    val annotationChecker: Option[AnnotationChecker] = None

    /**
     * Selectively activate this analyzer plugin and its associated annotation checker according
     * to the compiler phase.
     *
     * Note that the current phase can differ from the global compiler phase (look for `enteringPhase`
     * invocations in the compiler). For instance, lazy types created by the UnPickler are completed
     * at the phase in which their symbol is created. Observations show that this can even be the
     * parser phase. Since symbol completion can trigger subtyping, typing etc, your plugin might
     * need to be active also in phases other than namer and typer.
     *
     * Typically, this method can be implemented as
     *
     *   phase.id < global.currentRun.picklerPhase.id
     */
    def runIn(phase: Phase): Boolean = true

    /**
     * Let analyzer plugins change the expected type before type checking a tree.
     */
    def pluginsPt(typer: Typer, tree: Tree, mode: Int, pt: Type): Type = pt

    /**
     * Let analyzer plugins modify the type that has been computed for a tree.
     *
     * @param typer the yper that type checked `tree`
     * @param tree  the type-checked tree
     * @param mode  mode that was used for typing `tree`
     * @param pt    expected type that was used for typing `tree`
     * @param tpe   type inferred by the type checker, the same as `tree.tpe`
     */
    def pluginsTyped(typer: Typer, tree: Tree, mode: Int, pt: Type, tpe: Type): Type = tpe

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
     * The hooks into typeSig` allow analyzer plugins to add annotations to (or change the types
     * of) definition symbols. This cannot not be achieved by using `pluginsTyped`: this method
     * is only called during type checking, so changing the type of a symbol at this point is too
     * late: references to the symbol might already be typed and therefore obtain the the original
     * type assigned during naming.
     *
     * @param defTree is the definition for which the type was computed. The different cases are
     * outlined below. Note that this type is untyped (for methods and values with inferred type,
     * the typed rhs trees are availalble in analyzer.transformed).
     *
     * Case defTree: Template
     *   - typer: The typer for template members, i.e. expressins and definitions of defTree.body
     *   - pt   : WildcardType
     *   - tpe  : A ClassInfoType for the template
     *   - the class symbol is accessible through typer.context.owner
     *
     * Case defTree: ClassDef
     *   - typer: The typer for the class. Note that this typer has a different context than the
     *            typer for the template.
     *   - pt   : WildcardType
     *   - tpe  : A ClassInfoType, or a PolyType(params, ClassInfoType) for polymorphic classes.
     *            The class type is the one computed by templateSig, i.e. through the above case
     *
     * Case defTree: ModuleDef
     *   - typer: The typer for the module. context.owner of this typer is the module class symbol
     *   - pt   : WildcardType
     *   - tpe  : A ClassInfoType computed by templateSig
     *
     * Case defTree: DefDef
     *   - typer: The typer the rhs of this method
     *   - pt   : If tpt.isEmpty, either the result type from the overridden method, or WildcardType.
     *            Otherwise the type obtained from typing tpt.
     *   - tpe  : The type of the method (MethodType, PolyType or NullaryMethodType). (*)
     *
     * Case defTree: ValDef
     *   - typer: The typer for the rhs of this value
     *   - pt   : If tpt.isEmpty, WildcardType. Otherwise the type obtained from typing tpt.
     *   - tpe  : The type of this value. (*)
     *   - Note that pluginsTypeSig might be called multiple times for the same ValDef since it is
     *     used to compute the types of the accessor methods (see `pluginsTypeSigAccessor`)
     *
     *   typed-tpt.tpe if defined, computed type of rhs otherwise (in this case, typed tree available through transformed map)
     *
     * Case defTree: TypeDef
     *   - typer: The typer for the rhs of this type
     *   - pt   : WildcardType
     *   - tpe  : The type obtained from typing rhs (PolyType if the TypeDef defines a polymorphic type)
     */
    def pluginsTypeSig(typer: Typer, defTree: Tree, pt: Type, tpe: Type): Type = tpe

    /**
     * Modify the types of field accessors. The namer phase creates method types for getters and
     * setters based on the type of the corresponding field.
     *
     * Note: in order to compute the method type of an accessor, the namer calls `typeSig` on the
     * `ValDef` tree of the corresponding field. This implies that the `pluginsTypeSig` method
     * is potentially called multiple times for the same ValDef tree.
     *
     * @param typer The typer for the ValDef (not for the rhs)
     * @param tree The ValDef corresponding to the accessor
     * @param sym The accessor method symbol (getter, setter, beanGetter or beanSetter)
     * @param tpe The method type created by the namer for the accessor
     */
    def pluginsTypeSigAccessor(typer: Typer, tree: ValDef, sym: Symbol, tpe: Type): Type = tpe

    /**
     * Decide whether this analyzer plugin can adapt a tree that has an annotated type to the
     * given type tp, taking into account the given mode (see method adapt in trait Typers).
     */
    def canAdaptAnnotations(typer: Typer, tree: Tree, mode: Int, pt: Type): Boolean = false

    /**
     * Adapt a tree that has an annotated type to the given type tp, taking into account the given
     * mode (see method adapt in trait Typers).
     *
     * An implementation cannot rely on canAdaptAnnotations being called before. If the implementing
     * class cannot do the adaptiong, it should return the tree unchanged.
     */
    def adaptAnnotations(typer: Typer, tree: Tree, mode: Int, pt: Type): Tree = tree

    /**
     * Adapt the type of a return expression. The decision of a typer plugin whether the type
     * should be adapted is based on the type of the expression which is returned, as well as the
     * result type of the method (pt).
     *
     * By default, this method simply returns the passed `default` type.
     */
    def pluginsTypedReturn(typer: Typer, tree: Return, pt: Type, default: Type): Type = default
  }



  /** A list of registered analyzer plugins */
  private var analyzerPlugins: List[AnalyzerPlugin] = Nil

  /** The analyzer plugins which are active in the current phase. A subset of `analyzerPlugins` */
  private var activePlugins: List[AnalyzerPlugin] = Nil

  /** Registers a new analyzer plugin */
  def addAnalyzerPlugin(plugin: AnalyzerPlugin) {
    if (!analyzerPlugins.contains(plugin)) {
      analyzerPlugins = plugin :: analyzerPlugins
      setPluginsForPhase(global.phase)
    }
  }

  /**
   * Activates the analyzer plugins (and their annotation checkers) which should run
   * during `phase` according to their `runIn(phase)` method.
   */
  private def setPluginsForPhase(phase: Phase) {
    val (active, disabled) = analyzerPlugins.partition(_.runIn(phase))
    activePlugins = active
    disabled.flatMap(_.annotationChecker).foreach(removeAnnotationChecker)
    active.flatMap(_.annotationChecker).foreach(addAnnotationChecker)
  }

  /**
   * This method is invoked whenever the current compiler phase changes, i.e.
   *   - when the global phase advances
   *   - on calls to `enteringPhase` and friends
   */
  def pluginsEnterPhase(phase: Phase) {
    setPluginsForPhase(phase)
  }


  /** @see AnalyzerPlugin.pluginsPt */
  def pluginsPt(typer: Typer, tree: Tree, mode: Int, pt: Type): Type =
    activePlugins.foldLeft(pt)((pt, plugin) =>
      plugin.pluginsPt(typer, tree, mode, pt))

  /** @see AnalyzerPlugin.pluginsTyped */
  def pluginsTyped(typer: Typer, tree: Tree, mode: Int, pt: Type, tpe: Type): Type =
    activePlugins.foldLeft(tpe)((tpe, plugin) =>
      plugin.pluginsTyped(typer, tree, mode, pt, tpe))

  /** @see AnalyzerPlugin.pluginsTypeSig */
  def pluginsTypeSig(typer: Typer, defTree: Tree, pt: Type, tpe: Type): Type =
    activePlugins.foldLeft(tpe)((tpe, plugin) =>
      plugin.pluginsTypeSig(typer, defTree, pt, tpe))

  /** @see AnalyzerPlugin.pluginsTypeSigAccessor */
  def pluginsTypeSigAccessor(typer: Typer, tree: ValDef, sym: Symbol, tpe: Type): Type =
    activePlugins.foldLeft(tpe)((tpe, plugin) =>
      plugin.pluginsTypeSigAccessor(typer, tree, sym, tpe))

  /** @see AnalyzerPlugin.canAdaptAnnotations */
  def canAdaptAnnotations(typer: Typer, tree: Tree, mode: Int, pt: Type): Boolean =
    activePlugins.exists(_.canAdaptAnnotations(typer, tree, mode, pt))

  /** @see AnalyzerPlugin.adaptAnnotations */
  def adaptAnnotations(typer: Typer, tree: Tree, mode: Int, pt: Type): Tree =
    activePlugins.foldLeft(tree)((tree, plugin) =>
      plugin.adaptAnnotations(typer, tree, mode, pt))

  /**
   * @see AnalyzerPlugin.pluginsTypedReturn
   *
   * Note that the result is undefined if more than one annotation checker
   * returns an adapted type which is not a subtype of `default`.
   */
  def pluginsTypedReturn(typer: Typer, tree: Return, pt: Type, default: Type): Type = {
    val adaptedTypes = activePlugins flatMap { plugin =>
      val adapted = plugin.pluginsTypedReturn(typer, tree, pt, default)
      if (!(adapted <:< default)) List(adapted)
      else List()
    }
    adaptedTypes match {
      case fst :: _ => fst
      case List() => default
    }
  }
}
