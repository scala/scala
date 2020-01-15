package scala.tools.nsc.tasty.bridge

import scala.tools.nsc.tasty.{SafeEq, TastyUniverse}

trait AnnotationOps extends TastyKernel { self: TastyUniverse =>

  object Annotation {
    def deferredSymAndTree(annotee: Symbol)(symf: => Symbol)(tree: => Tree)(implicit ctx: Contexts.Context): Annotation =
      new symbolTable.LazyAnnotationInfo({
        val annotSym = symf
        val isChild = defn.childAnnotationClass.exists(annotSym === _)
        if (annotSym.isNonBottomSubClass(defn.StaticAnnotationClass) || annotSym.isJavaDefined || isChild) {
          val annotTree = tree
          ctx.log(s"annotation of $annotee = $annotTree")
          if (isChild) symbolTable.AnnotationInfo(annotTree.tpe, Nil, Nil)
          else mkAnnotation(annotTree)
        }
        else {
          ctx.log(s"Ignoring non-static annotation $annotSym")
          symbolTable.UnmappableAnnotation
        }
      })
  }

}
