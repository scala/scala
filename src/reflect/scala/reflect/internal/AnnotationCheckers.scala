/* NSC -- new Scala compiler
 * Copyright 2007-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package internal

/** Additions to the type checker that can be added at
 *  run time.  Typically these are added by
 *  compiler plugins. */
trait AnnotationCheckers {
  self: SymbolTable =>


  /** An additional checker for annotations on types.
   *  Typically these are registered by compiler plugins
   *  with the addAnnotationChecker method. */
  abstract class AnnotationChecker {
    /** Check the annotations on two types conform. */
    def annotationsConform(tpe1: Type, tpe2: Type): Boolean

    /** Refine the computed least upper bound of a list of types.
     *  All this should do is add annotations. */
    def annotationsLub(tp: Type, ts: List[Type]): Type = tp

    /** Refine the computed greatest lower bound of a list of types.
     *  All this should do is add annotations. */
    def annotationsGlb(tp: Type, ts: List[Type]): Type = tp

    /** Refine the bounds on type parameters to the given type arguments. */
    def adaptBoundsToAnnotations(bounds: List[TypeBounds],
      tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = bounds

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */
    def addAnnotations(tree: Tree, tpe: Type): Type = tpe

    /** Decide whether this annotation checker can adapt a tree
     *  that has an annotated type to the given type tp, taking
     *  into account the given mode (see method adapt in trait Typers).*/
    def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = false

    /** Adapt a tree that has an annotated type to the given type tp,
     *  taking into account the given mode (see method adapt in trait Typers).
     *  An implementation cannot rely on canAdaptAnnotations being called
     *  before. If the implementing class cannot do the adaptiong, it
     *  should return the tree unchanged.*/
    def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = tree
  }

  // Syncnote: Annotation checkers inaccessible to reflection, so no sync in var necessary.
  /** The list of annotation checkers that have been registered */
  private var annotationCheckers: List[AnnotationChecker] = Nil

  /** Register an annotation checker.  Typically these
   *  are added by compiler plugins. */
  def addAnnotationChecker(checker: AnnotationChecker) {
    if (!(annotationCheckers contains checker))
      annotationCheckers = checker :: annotationCheckers
  }

  /** Remove all annotation checkers */
  def removeAllAnnotationCheckers() {
    annotationCheckers = Nil
  }

  /** Check that the annotations on two types conform.  To do
   *  so, consult all registered annotation checkers. */
  def annotationsConform(tp1: Type, tp2: Type): Boolean = {
    /* Finish quickly if there are no annotations */
    if (tp1.annotations.isEmpty && tp2.annotations.isEmpty)
      true
    else
     annotationCheckers.forall(
       _.annotationsConform(tp1,tp2))
  }

  /** Refine the computed least upper bound of a list of types.
   *  All this should do is add annotations. */
  def annotationsLub(tpe: Type, ts: List[Type]): Type = {
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.annotationsLub(tpe, ts))
  }

  /** Refine the computed greatest lower bound of a list of types.
   *  All this should do is add annotations. */
  def annotationsGlb(tpe: Type, ts: List[Type]): Type = {
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.annotationsGlb(tpe, ts))
  }

  /** Refine the bounds on type parameters to the given type arguments. */
  def adaptBoundsToAnnotations(bounds: List[TypeBounds],
    tparams: List[Symbol], targs: List[Type]): List[TypeBounds] = {
      annotationCheckers.foldLeft(bounds)((bounds, checker) =>
        checker.adaptBoundsToAnnotations(bounds, tparams, targs))
  }

  /** Let all annotations checkers add extra annotations
   *  to this tree's type. */
  def addAnnotations(tree: Tree, tpe: Type): Type = {
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.addAnnotations(tree, tpe))
  }

  /** Find out whether any annotation checker can adapt a tree
   *  to a given type. Called by Typers.adapt. */
  def canAdaptAnnotations(tree: Tree, mode: Int, pt: Type): Boolean = {
    annotationCheckers.exists(_.canAdaptAnnotations(tree, mode, pt))
  }

  /** Let registered annotation checkers adapt a tree
   *  to a given type (called by Typers.adapt). Annotation checkers
   *  that cannot do the adaption should pass the tree through
   *  unchanged. */
  def adaptAnnotations(tree: Tree, mode: Int, pt: Type): Tree = {
    annotationCheckers.foldLeft(tree)((tree, checker) =>
      checker.adaptAnnotations(tree, mode, pt))
  }
}
