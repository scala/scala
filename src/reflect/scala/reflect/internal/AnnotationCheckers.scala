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

package scala
package reflect
package internal

/** Additions to the type checker that can be added at
 *  run time.  Typically these are added by
 *  compiler plugins. */
trait AnnotationCheckers {
  self: SymbolTable =>


  /** An additional checker for annotations on types.
   *  Typically these are registered by compiler plugins
   *  with the addAnnotationChecker method. */
  trait AnnotationChecker {

    /**
     * Selectively activate this annotation checker. When using both an annotation checker
     * and an analyzer plugin, it is common to run both of them only during selected
     * compiler phases. See documentation in AnalyzerPlugin.isActive.
     */
    def isActive(): Boolean = true

    /** Check the annotations on two types conform. */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean

    /** Refine the computed least upper bound of a list of types.
     *  All this should do is add annotations. */
    def annotationsLub(tp: Type, ts: List[Type]): Type = tp

    /** Refine the computed greatest lower bound of a list of types.
     *  All this should do is add annotations. */
    def annotationsGlb(tp: Type, ts: List[Type]): Type = tp

    /** Refine the bounds on type parameters to the given type arguments. */
    def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol],
                                 targs: List[Type]): List[TypeBounds] = bounds

    /**
     * Modify the type that has thus far been inferred for a tree. All this should
     * do is add annotations.
     */
    @deprecatedOverriding("create an AnalyzerPlugin and use pluginsTyped", "2.10.1")
    def addAnnotations(tree: Tree, tpe: Type): Type = tpe

    /**
     * Decide whether this analyzer plugin can adapt a tree that has an annotated type to the
     * given type tp, taking into account the given mode (see method adapt in trait Typers).
     */
    @deprecatedOverriding("create an AnalyzerPlugin and use canAdaptAnnotations", "2.10.1")
    def canAdaptAnnotations(tree: Tree, mode: Mode, pt: Type): Boolean = false

    /**
     * Adapt a tree that has an annotated type to the given type tp, taking into account the given
     * mode (see method adapt in trait Typers).
     *
     * An implementation cannot rely on canAdaptAnnotations being called before. If the implementing
     * class cannot do the adapting, it should return the tree unchanged.
     */
    @deprecatedOverriding("create an AnalyzerPlugin and use adaptAnnotations", "2.10.1")
    def adaptAnnotations(tree: Tree, mode: Mode, pt: Type): Tree = tree

    /**
     * Adapt the type of a return expression. The decision of a typer plugin whether the type
     * should be adapted is based on the type of the expression which is returned, as well as the
     * result type of the method (pt).
     *
     * By default, this method simply returns the passed `default` type.
     */
    @deprecatedOverriding(
      "Create an AnalyzerPlugin and use pluginsTypedReturn. Note: the 'tree' argument here is\n"+
      "the 'expr' of a Return tree; 'pluginsTypedReturn' takes the Return tree itself as argument", "2.10.1")
    def adaptTypeOfReturn(tree: Tree, pt: Type, default: => Type): Type = default
  }

  // Syncnote: Annotation checkers inaccessible to reflection, so no sync in var necessary.

  /** The list of annotation checkers that have been registered */
  private[this] var annotationCheckers: List[AnnotationChecker] = Nil

  /** Register an annotation checker.  Typically these are added by compiler plugins. */
  def addAnnotationChecker(checker: AnnotationChecker): Unit = {
    if (!(annotationCheckers contains checker))
      annotationCheckers = checker :: annotationCheckers
  }

  /** Remove all annotation checkers */
  def removeAllAnnotationCheckers(): Unit = {
    annotationCheckers = Nil
  }

  /** @see AnnotationChecker.annotationsConform */
  def annotationsConform(tp1: Type, tp2: Type): Boolean =
    if (annotationCheckers.isEmpty || (tp1.annotations.isEmpty && tp2.annotations.isEmpty)) true
    else annotationCheckers.forall(checker => {
      !checker.isActive() || checker.annotationsConform(tp1,tp2)
    })

  /** @see AnnotationChecker.annotationsLub */
  def annotationsLub(tpe: Type, ts: List[Type]): Type =
    if (annotationCheckers.isEmpty) tpe
    else annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      if (!checker.isActive()) tpe else checker.annotationsLub(tpe, ts))

  /** @see AnnotationChecker.annotationsGlb */
  def annotationsGlb(tpe: Type, ts: List[Type]): Type =
    if (annotationCheckers.isEmpty) tpe
    else annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      if (!checker.isActive()) tpe else checker.annotationsGlb(tpe, ts))

  /** @see AnnotationChecker.adaptBoundsToAnnotations */
  def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol],
                               targs: List[Type]): List[TypeBounds] =
    if (annotationCheckers.isEmpty) bounds
    else annotationCheckers.foldLeft(bounds)((bounds, checker) =>
      if (!checker.isActive()) bounds else checker.adaptBoundsToAnnotations(bounds, tparams, targs))


  /* The following methods will be removed with the deprecated methods is AnnotationChecker. */

  def addAnnotations(tree: Tree, tpe: Type): Type =
    if (annotationCheckers.isEmpty) tpe
    else annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      if (!checker.isActive()) tpe else checker.addAnnotations(tree, tpe))

  def canAdaptAnnotations(tree: Tree, mode: Mode, pt: Type): Boolean =
    if (annotationCheckers.isEmpty) false
    else annotationCheckers.exists(checker => {
      checker.isActive() && checker.canAdaptAnnotations(tree, mode, pt)
    })

  def adaptAnnotations(tree: Tree, mode: Mode, pt: Type): Tree =
    if (annotationCheckers.isEmpty) tree
    else annotationCheckers.foldLeft(tree)((tree, checker) =>
      if (!checker.isActive()) tree else checker.adaptAnnotations(tree, mode, pt))

  def adaptTypeOfReturn(tree: Tree, pt: Type, default: => Type): Type =
    if (annotationCheckers.isEmpty) default
    else annotationCheckers.foldLeft(default)((tpe, checker) =>
      if (!checker.isActive()) tpe else checker.adaptTypeOfReturn(tree, pt, tpe))
}
