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
    case _ =>
      throw new Exception(s"unexpected annotation kind from TASTy: ${u.showRaw(tree)}")
  }

  final class DeferredAnnotation(tree: Symbol => Context => Tree) {
    private[bridge] def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo = {
      val atree = tree(annotee)(ctx)
      ctx.log(s"annotation of $annotee = $atree")
      mkAnnotation(atree)
    }
    private[bridge] def lzy(annotee: Symbol)(implicit ctx: Context): u.LazyAnnotationInfo = {
      u.AnnotationInfo.lazily {
        eager(annotee)
      }
    }
  }

}
