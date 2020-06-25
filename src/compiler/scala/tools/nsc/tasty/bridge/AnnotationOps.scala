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

import scala.tools.nsc.tasty.{SafeEq, TastyUniverse}

/** Adds support for creating annotations from Trees */
trait AnnotationOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  private[bridge] final def mkAnnotation(tree: Tree): u.Annotation = tree match {
    case u.Apply(u.Select(u.New(tpt), u.nme.CONSTRUCTOR), args) =>
      u.AnnotationInfo(tpt.tpe, args, Nil)
    case _ =>
      throw new Exception("unexpected annotation kind from TASTy")
  }

  final class DeferredAnnotation(annotSym: Symbol, tree: Symbol => Context => Either[String, Tree]) {
    private[bridge] def mkLazyAnnotation(annotee: Symbol)(implicit ctx: Context): u.Annotation = {
      lazy val tree0 = tree(annotee)(ctx)
      u.AnnotationInfo.lazily {
        tree0 match {
          case Left(err) =>
            u.reporter.error(u.NoPosition, err)
            u.UnmappableAnnotation
          case Right(annotTree) =>
            val isChild = ctx.optionalClass(tpnme.ScalaAnnotationInternal_Child).exists(annotSym === _)
            ctx.log(s"annotation of $annotee = $annotTree")
            if (isChild) u.AnnotationInfo(annotTree.tpe, Nil, Nil)
            else mkAnnotation(annotTree)
        }
      }
    }
  }

}
