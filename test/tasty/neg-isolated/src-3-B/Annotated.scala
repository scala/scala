package tastytest

@annot
trait Annotated

@publicAnnot(new Parent)
trait PublicAnnotated

@symbollicAnnot(new <<<)
trait SymbollicAnnotated

@publicSymbollicAnnot(new >>>)
trait PublicSymbollicAnnotated

@publicPkgAnnot(new Member)
trait PublicPkgAnnotated
