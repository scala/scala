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

import scala.tools.nsc.tasty.TastyUniverse

/** Adds support for creating annotations from Trees */
trait AnnotationOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  private[bridge] final def mkAnnotation(tree: Tree): u.Annotation = tree match {
    case u.Apply(u.Select(u.New(tpt), u.nme.CONSTRUCTOR), args) =>
      u.AnnotationInfo(tpt.tpe, args, Nil)
    case u.Apply(u.TypeApply(u.Select(u.New(tpt), u.nme.CONSTRUCTOR), tpargs), args) =>
      u.AnnotationInfo(u.appliedType(tpt.tpe, tpargs.map(_.tpe)), args, Nil)
    case u.New(tpt) =>
      // this is to handle incorrectly formatted annotations in dotty - https://github.com/lampepfl/dotty/issues/10113
      u.AnnotationInfo(tpt.tpe, Nil, Nil)
    case _ =>
      throw new Exception(s"unexpected annotation kind from TASTy: ${u.showRaw(tree)}")
  }

  abstract class DeferredAnnotation {
    private[bridge] def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo
    private[bridge] def lzy(annotee: Symbol)(implicit ctx: Context): u.LazyAnnotationInfo = {
      u.AnnotationInfo.lazily {
        eager(annotee)
      }
    }
  }

  object DeferredAnnotation {

    def fromTree(tree: Symbol => Context => Tree) =
      new FromTree(tree)

    class FromTree(tree: Symbol => Context => Tree) extends DeferredAnnotation {
      private[bridge] def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo = {
        val atree = tree(annotee)(ctx)
        ctx.log(s"annotation on $annotee: $atree")
        val annot = mkAnnotation(atree)
        val annotSym = annot.tpe.typeSymbol
        if ((annotSym eq defn.TargetNameAnnotationClass) || (annotSym eq defn.StaticMethodAnnotationClass)) {
          annotee.addAnnotation(
            u.definitions.CompileTimeOnlyAttr,
            u.Literal(u.Constant(unsupportedMessage(s"annotation on $annotee: @$annot"))))
        }
        annot
      }
    }

  }

}
