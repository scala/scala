case class Foo(x: Int)

// 1) Checking typer specifically:
//
// [Now checking: typer]
// work/fail7.scala:1: error: double definition:
// method canEqual:(x$1: Any)Boolean and
// method canEqual:(x$1: Any)Boolean at line 1
// have same type
// case class Foo(x: Int)
//            ^
//
// 2) Checking all, which somehow misses it until superaccessors:
//
// [Not checkable: parser]
// [Not checkable: namer]
// [Not checkable: packageobjects]
// [Now checking: typer]
// [Now checking: superaccessors]
// work/fail7.scala:1: error: 
// **** ERROR DURING INTERNAL CHECKING ****
// method canEqual is defined twice
// case class Foo(x: Int)
//            ^
// one error found
//
// 3) Checking uncurry:
//
// [Now checking: uncurry]
// work/fail7.scala:1: error: double definition:
// method canEqual:(x$1: Any)Boolean and
// method canEqual:(x$1: Any)Boolean at line 1
// have same type
// case class Foo(x: Int)
//            ^
// exception when typing Foo.this.productArity()
// Foo.this.productArity of type Int does not take parameters in file work/fail7.scala
// scala.tools.nsc.symtab.Types$TypeError: Foo.this.productArity of type Int does not take parameters
//  at scala.tools.nsc.typechecker.Contexts$Context.error(Contexts.scala:277)
//  at scala.tools.nsc.typechecker.Infer$Inferencer.error(Infer.scala:205)
//  at scala.tools.nsc.typechecker.Infer$Inferencer.errorTree(Infer.scala:209)
//  at scala.tools.nsc.typechecker.Typers$Typer.doTypedApply(Typers.scala:2632)
//  at scala.tools.nsc.typechecker.Typers$Typer.typedApply$1(Typers.scala:3400)
//  at scala.tools.nsc.typechecker.Typers$Typer.typed1(Typers.scala:4069)
//  at scala.tools.nsc.transform.Erasure$Eraser.liftedTree1$1(Erasure.scala:663)
//  at scala.tools.nsc.transform.Erasure$Eraser.typed1(Erasure.scala:662)
//  at scala.tools.nsc.typechecker.Typers$Typer.typed(Typers.scala:4223)
//  at scala.tools.nsc.typechecker.Typers$Typer.transformedOrTyped(Typers.scala:4368)
//  at scala.tools.nsc.typechecker.Typers$Typer.typedDefDef(Typers.scala:1796)
//
// 4) Checking constructors:
//
// [Now checking: constructors]
// work/fail7.scala:1: error: 
// **** ERROR DURING INTERNAL CHECKING ****
// value x in class Foo cannot be accessed in Foo
//  because of an internal error (no accessible symbol):
// sym = value x
// underlying(sym) = value x
// pre = Foo
// site = Foo.this
// tree = Foo.this.x
// sym.accessBoundary(sym.owner) = class Foo
// sym.ownerChain = List(value x, class Foo, package <empty>, package <root>)
// sym.owner.thisType = Foo
// context.owner = package <empty>
// context.outer.enclClass.owner = package <empty>
// case class Foo(x: Int)
//                ^
// one error found
