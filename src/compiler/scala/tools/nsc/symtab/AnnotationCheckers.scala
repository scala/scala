/* NSC -- new Scala compiler
 * Copyright 2007-2008 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id: AnnotationInfos.scala 13367 2007-11-28 05:17:14Z spoon $

package scala.tools.nsc.symtab

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

    /** Modify the type that has thus far been inferred
     *  for a tree.  All this should do is add annotations. */
    def addAnnotations(tree: Tree, tpe: Type): Type = tpe
  }

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
    /* Finish quickly if there are no attributes */
    if (tp1.attributes.isEmpty && tp2.attributes.isEmpty)
      true
    else
     annotationCheckers.forall(
       _.annotationsConform(tp1,tp2))
  }

  /** Let all annotations checkers add extra annotations
   *  to this tree's type. */
  def addAnnotations(tree: Tree, tpe: Type): Type = {
    annotationCheckers.foldLeft(tpe)((tpe, checker) =>
      checker.addAnnotations(tree, tpe))
  }
}
