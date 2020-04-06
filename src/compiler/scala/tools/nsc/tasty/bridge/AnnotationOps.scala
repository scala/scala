package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.{SafeEq, TastyUniverse}

trait AnnotationOps { self: TastyUniverse =>
  import self.{symbolTable => u}

  object Annotation {
    def deferredSymAndTree(annotee: Symbol)(symf: => Symbol)(tree: Context => Tree)(implicit ctx: Context): Annotation =
      new symbolTable.LazyAnnotationInfo({
        val annotSym = symf
        val annotTree = tree(ctx)
        val isChild = defn.childAnnotationClass.exists(annotSym === _)
        ctx.log(s"annotation of $annotee = $annotTree")
        if (isChild) u.AnnotationInfo(annotTree.tpe, Nil, Nil)
        else mkAnnotation(annotTree)
      })
  }

}
