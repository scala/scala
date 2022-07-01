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

  private[bridge] final def mkAnnotation(tree: Tree): u.Annotation = {
    def go(tpargs: List[Type], args: List[Tree], tree: Tree): u.Annotation = tree match {
      case u.Select(u.New(tpt), u.nme.CONSTRUCTOR) =>
        val atp = if (tpargs.isEmpty) tpt.tpe else u.appliedType(tpt.tpe, tpargs)
        u.AnnotationInfo(atp, args, Nil)
      case u.TypeApply(pre, newTpArgs) if tpargs.isEmpty =>
        go(newTpArgs.map(_.tpe), args, pre)
      case u.Apply(pre, Nil) => // skip the empty term param list
        go(tpargs, args, pre)
      case u.Apply(pre, newArgs) if args.isEmpty =>
        go(tpargs, newArgs, pre)
      case _ =>
        throw new Exception(s"unexpected annotation kind from TASTy: ${u.showRaw(tree)}")
    }
    tree match {
      case u.New(tpt) =>
        // this is to handle incorrectly formatted annotations in dotty - https://github.com/lampepfl/dotty/issues/10113
        u.AnnotationInfo(tpt.tpe, Nil, Nil)
      case _ =>
        go(Nil, Nil, tree)
    }
  }

  sealed abstract class DeferredAnnotation {

    private[bridge] def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo
    private[bridge] final def lzy(annotee: Symbol)(implicit ctx: Context): u.LazyAnnotationInfo = {
      u.AnnotationInfo.lazily(eager(annotee))
    }
  }

  object DeferredAnnotation {

    def fromTree(tree: Symbol => Context => Tree): DeferredAnnotation = {
      new DeferredAnnotation {
        private[bridge] final def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo = {
          val atree = tree(annotee)(ctx)
          mkAnnotation(atree)
        }
      }
    }
  }

}
