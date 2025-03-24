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

package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.TastyUniverse

/** Adds support for creating annotations from Trees */
trait AnnotationOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  trait ShowKind[T] {
    def showKind(annot: String, t: T)(implicit ctx: Context): String
  }

  object ShowKind {
    implicit object ShowSymbol extends ShowKind[u.Symbol] {
      def showKind(annot: String, t: u.Symbol)(implicit ctx: Context): String = s"$annot ${location(t)}"
    }
    implicit object ShowType extends ShowKind[u.Type] {
      def showKind(annot: String, t: u.Type)(implicit ctx: Context): String =
        s"type ${showType(t, wrap = false)} $annot of ${location(ctx.owner)}"
    }
  }

  private[bridge] final def mkAnnotation[T: ShowKind](tree: Tree, annotee: T)(implicit ctx: Context): u.Annotation = {
    def go(tpargs: List[Type], args: List[List[Tree]], tree: Tree): u.Annotation = tree match {
      case u.Select(u.New(tpt), u.nme.CONSTRUCTOR) =>
        val atp = if (tpargs.isEmpty) tpt.tpe else u.appliedType(tpt.tpe, tpargs)
        if (args.lengthIs > 1) {
          val soFar = s"@${atp.typeSymbol.name.toString}${args.map(_.mkString("(", ", ", ")")).mkString("")}"
          u.reporter.warning(u.NoPosition,
            "Implementation limitation: multiple argument lists on annotations are\n"+
            "currently not supported; ignoring arguments " + args(1) + " on\n"+
           s"${implicitly[ShowKind[T]].showKind(soFar, annotee)}")
        }
        u.AnnotationInfo(atp, args.headOption.getOrElse(Nil), Nil)
      case u.TypeApply(pre, newTpArgs) if tpargs.isEmpty =>
        go(newTpArgs.map(_.tpe), args, pre)
      case u.Apply(pre, Nil) => // skip the empty term param list
        go(tpargs, args, pre)
      case u.Apply(pre, newArgs) =>
        go(tpargs, newArgs :: args, pre)
      case _ =>
        throw new Exception(s"unexpected annotation kind from TASTy: ${u.showRaw(tree)}")
    }
    tree match {
      case u.New(tpt) =>
        // this is to handle incorrectly formatted annotations in dotty - https://github.com/scala/scala3/issues/10113
        u.AnnotationInfo(tpt.tpe, Nil, Nil)
      case _ =>
        go(Nil, Nil, tree)
    }
  }

  sealed abstract class DeferredAnnotation(annotSym: Symbol) {

    protected def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo
    private[bridge] final def lzy(annotee: Symbol)(implicit ctx: Context): u.LazyAnnotationInfo = {
      u.AnnotationInfo.lazily(annotSym, eager(annotee))
    }
  }

  object DeferredAnnotation {

    def fromTree(annotSym: Symbol)(tree: Symbol => Context => Tree): DeferredAnnotation = {
      new DeferredAnnotation(annotSym) {
        protected final def eager(annotee: Symbol)(implicit ctx: Context): u.AnnotationInfo = {
          val atree = tree(annotee)(ctx)
          mkAnnotation(atree, annotee)
        }
      }
    }
  }

}
