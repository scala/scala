
class X { private[p] class C(i: Int = 42) ; def c = new C(17) }

class Y {
  private[p] class D(i: Int = 42) { def f = new Q }
  class Z { def d = new D() }
}

/*
 *
test/files/pos/t10748.scala:2: error: p is not an enclosing class
class X { private[p] class C(i: Int = 42) ; def c = new C(17) }
                           ^
error: java.lang.AssertionError: assertion failed:
  C
     while compiling: test/files/pos/t10748.scala
        during phase: globalPhase=typer, enteringPhase=namer
     library version: version 2.13.0-20180714-072842-414f884
    compiler version: version 2.13.0-20180714-072842-414f884
  reconstructed args: -d /tmp

  last tree to typer: TypeTree(class C)
       tree position: line 2 of test/files/pos/t10748.scala
            tree tpe: X.this.C
              symbol: class C in class X
   symbol definition: class C extends AnyRef (a ClassSymbol)
      symbol package: <empty>
       symbol owners: class C -> class X
           call site: constructor C in class C in package <empty>

== Source file context for tree position ==

     1
     2 class X { private[p] class C(i: Int = 42) ; def c = new C(17) }
     3
     4
	at scala.reflect.internal.SymbolTable.throwAssertionError(SymbolTable.scala:162)
	at scala.reflect.internal.SymbolTable.assert(SymbolTable.scala:139)
	at scala.reflect.internal.Symbols$Symbol.info(Symbols.scala:1508)
	at scala.reflect.internal.Symbols$Symbol.initialize(Symbols.scala:1675)
	at scala.tools.nsc.typechecker.Namers$Namer$DefaultGetterInCompanion.<init>(Namers.scala:1601)
	at scala.tools.nsc.typechecker.Namers$Namer$DefaultGetterNamerSearch$.apply(Namers.scala:1591)
	at scala.tools.nsc.typechecker.Namers$Namer.addDefaultGetters(Namers.scala:1483)
	at scala.tools.nsc.typechecker.Namers$Namer.methodSig(Namers.scala:1396)
	at scala.tools.nsc.typechecker.Namers$Namer.memberSig(Namers.scala:1859)
	at scala.tools.nsc.typechecker.Namers$Namer.typeSig(Namers.scala:1825)
	at scala.tools.nsc.typechecker.Namers$Namer$MonoTypeCompleter.completeImpl(Namers.scala:849)
	at scala.tools.nsc.typechecker.Namers$LockingTypeCompleter.complete(Namers.scala:2009)
	at scala.tools.nsc.typechecker.Namers$LockingTypeCompleter.complete$(Namers.scala:2007)
	at scala.tools.nsc.typechecker.Namers$TypeCompleterBase.complete(Namers.scala:2002)
	at scala.reflect.internal.Symbols$Symbol.info(Symbols.scala:1527)
	at scala.reflect.internal.Symbols.argsDependOnPrefix(Symbols.scala:3763)
	at scala.reflect.internal.Symbols.argsDependOnPrefix$(Symbols.scala:3751)
	at scala.reflect.internal.SymbolTable.argsDependOnPrefix(SymbolTable.scala:17)
	at scala.tools.nsc.typechecker.Typers$Typer.typedSelect$1(Typers.scala:5046)
 */
