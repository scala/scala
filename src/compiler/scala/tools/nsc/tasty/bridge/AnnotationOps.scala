package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.{SafeEq, TastyUniverse}

trait AnnotationOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  final def mkAnnotation(tree: Tree): Annotation = tree match {
    case u.Apply(u.Select(u.New(tpt), u.nme.CONSTRUCTOR), args) =>
      u.AnnotationInfo(tpt.tpe, args, Nil)
    case _ =>
      throw new Exception("unexpected annotation kind from TASTy")
  }

  object Annotation {
    def deferredSymAndTree(annotee: Symbol)(symf: Context => Symbol)(tree: Context => Either[String, Tree])(implicit ctx: Context): Annotation =
      new symbolTable.LazyAnnotationInfo({
        val annotSym = symf(ctx)
        tree(ctx) match {
          case Left(err) =>
            u.reporter.error(u.NoPosition, err)
            u.UnmappableAnnotation
          case Right(annotTree) =>
            val isChild = defn.childAnnotationClass.exists(annotSym === _)
            ctx.log(s"annotation of $annotee = $annotTree")
            if (isChild) u.AnnotationInfo(annotTree.tpe, Nil, Nil)
            else mkAnnotation(annotTree)
        }
      })
  }

}
