/* NSC -- new Scala compiler
 * Copyright 2007-2013 LAMP/EPFL
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
    def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol],
                                 targs: List[Type]): List[TypeBounds] = bounds
  }

  // Syncnote: Annotation checkers inaccessible to reflection, so no sync in var necessary.

  /** The list of annotation checkers that have been registered */
  private var annotationCheckers: List[AnnotationChecker] = Nil

  /** Register an annotation checker.  Typically these are added by compiler plugins. */
  def addAnnotationChecker(checker: AnnotationChecker) {
    if (!(annotationCheckers contains checker))
      annotationCheckers = checker :: annotationCheckers
  }

  /** Remove an annotation checker */
  def removeAnnotationChecker(checker: AnnotationChecker) {
    annotationCheckers = annotationCheckers.filterNot(_ == checker)
  }

  /** Remove all annotation checkers */
  def removeAllAnnotationCheckers() {
    annotationCheckers = Nil
  }

  /** @see AnnotationChecker.annotationsConform */
  def annotationsConform(tp1: Type, tp2: Type): Boolean =
    /* Finish quickly if there are no annotations */
    if (tp1.annotations.isEmpty && tp2.annotations.isEmpty)
      true
    else
      annotationCheckers.forall(
       _.annotationsConform(tp1,tp2))

  /** @see AnnotationChecker.annotationsLub */
  def annotationsLub(tpe: Type, ts: List[Type]): Type =
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.annotationsLub(tpe, ts))

  /** @see AnnotationChecker.annotationsGlb */
  def annotationsGlb(tpe: Type, ts: List[Type]): Type =
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.annotationsGlb(tpe, ts))

  /** @see AnnotationChecker.adaptBoundsToAnnotations */
  def adaptBoundsToAnnotations(bounds: List[TypeBounds], tparams: List[Symbol],
                               targs: List[Type]): List[TypeBounds] =
    annotationCheckers.foldLeft(bounds)((bounds, checker) =>
      checker.adaptBoundsToAnnotations(bounds, tparams, targs))
}
