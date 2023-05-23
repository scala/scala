package scala.reflect.internal

import org.junit.Assert._
import org.junit.{After, Assert, Before, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class TypesTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  @Test
  def testRefinedTypeSI8611(): Unit = {
    def stringNarrowed = StringTpe.narrow
    assert(stringNarrowed != stringNarrowed)
    assert(!(stringNarrowed =:= stringNarrowed))

    def boolWithString = refinedType(BooleanTpe :: StringTpe :: Nil, NoSymbol)
    assert(boolWithString != boolWithString)
    assert(boolWithString =:= boolWithString)

    val boolWithString1 = boolWithString
    val boolWithString1narrow1 = boolWithString1.narrow
    val boolWithString1narrow2 = boolWithString1.narrow
    // Two narrowings of the same refinement end up =:=. This was the root
    // cause of scala/bug#8611. See `narrowUniquely` in `Logic` for the workaround.
    assert(boolWithString1narrow1 =:= boolWithString1narrow2)
    val uniquelyNarrowed1 = refinedType(boolWithString1narrow1 :: Nil, NoSymbol)
    val uniquelyNarrowed2 = refinedType(boolWithString1narrow2 :: Nil, NoSymbol)
    assert(uniquelyNarrowed1 =:= uniquelyNarrowed2)
  }

  @Test
  def testTransitivityWithModuleTypeRef(): Unit = {
    import rootMirror.EmptyPackageClass
    val (module, moduleClass) = EmptyPackageClass.newModuleAndClassSymbol(TermName("O"), NoPosition, 0L)
    val minfo = ClassInfoType(List(ObjectTpe), newScope, moduleClass)
    module.moduleClass setInfo minfo
    module setInfo module.moduleClass.tpe
    val tp1 = TypeRef(ThisType(EmptyPackageClass), moduleClass, Nil)
    val tp2 = SingleType(ThisType(EmptyPackageClass), module)
    val tp3 = ThisType(moduleClass)
    val tps = List(tp1, tp2, tp3)
    val results = mutable.Buffer[String]()
    tps.permutations.foreach {
      case ts @ List(a, b, c) =>
        def tsShownRaw = ts.map(t => showRaw(t)).mkString(", ")
        if (a <:< b && b <:< c && !(a <:< c)) results += s"<:< intransitive: $tsShownRaw"
        if (a =:= b && b =:= c && !(a =:= c)) results += s"=:= intransitive: $tsShownRaw"
    }
    results.toList match {
      case Nil => // okay
      case xs =>
        Assert.fail(xs.mkString("\n"))
    }
  }

  @Test
  def testRefinementContains(): Unit = {
    val refinement = typeOf[{def foo: Int}]
    assert(refinement.isInstanceOf[RefinedType])
    assert(refinement.contains(IntClass))
    val elem0 = refinement.baseTypeSeq(0)
    assert(elem0.isInstanceOf[RefinementTypeRef])
    assert(elem0.contains(IntClass))
  }

  @Test
  def testRefinedLubs(): Unit = {
    // https://github.com/scala/scala-dev/issues/168
    assertEquals(typeOf[Option[AnyVal]], lub(typeOf[Option[Int] with Option[Char]] :: typeOf[Option[Boolean] with Option[Short]] :: Nil))
    assertEquals(typeOf[Option[AnyVal]], lub(typeOf[Option[Int] with Option[Char]] :: typeOf[Option[Boolean]] :: Nil))
    assertEquals(typeOf[Option[AnyVal]], lub((typeOf[Option[Int] with Option[Char]] :: typeOf[Option[Boolean] with Option[Short]] :: Nil).reverse))
    assertEquals(typeOf[Option[AnyVal]], lub((typeOf[Option[Int] with Option[Char]] :: typeOf[Option[Boolean]] :: Nil).reverse))
  }

  @Test
  def testExistentialRefinement(): Unit = {
    import rootMirror.EmptyPackageClass

    // class M[A]
    val MClass = EmptyPackageClass.newClass("M")
    val A = MClass.newTypeParameter("A").setInfo(TypeBounds.empty)
    MClass.setInfo(PolyType(A :: Nil, ClassInfoType(ObjectClass.tpeHK :: Nil, newScopeWith(), MClass)))

    // (M[Int] with M[X] { def m: Any }) forSome { type X }
    val X = NoSymbol.newExistential("X").setInfo(TypeBounds.empty)
    val T: Type = {
      val decls = newScopeWith(MClass.newMethod("m").setInfo(NullaryMethodType(AnyClass.tpeHK)))
      val refined = refinedType(appliedType(MClass, IntClass.tpeHK) :: appliedType(MClass, X.tpeHK) :: Nil, NoSymbol, decls, NoPosition)
      newExistentialType(X :: Nil, refined)
    }

    val RefinementClass = T.underlying.typeSymbol
    assertTrue(RefinementClass.isRefinementClass)
    TypeRef(NoPrefix, RefinementClass, Nil) match {
      case rtr : RefinementTypeRef =>
        // ContainsCollector needs to look inside the info of symbols of RefinementTypeRefs
        assert(rtr.contains(X))
    }

    val underlying = T.underlying
    val baseTypeSeqIndices = T.baseTypeSeq.toList.indices
    for (i <- baseTypeSeqIndices) {
      // Elements of the existential type should have the same type symbol as underlying
      assertEquals(T.baseTypeSeq.typeSymbol(i), underlying.baseTypeSeq.typeSymbol(i))
    }

    // Type symbols should be distinct
    def checkDistinctTypeSyms(bts: BaseTypeSeq): Unit = {
      val syms = baseTypeSeqIndices.map(T.baseTypeSeq.typeSymbol)
      assertEquals(syms, syms.distinct)
    }
    checkDistinctTypeSyms(T.baseTypeSeq)
    checkDistinctTypeSyms(T.underlying.baseTypeSeq)

    // This is the entry for the refinement class
    assertTrue(T.baseTypeSeq.typeSymbol(0).isRefinementClass)
    assertEquals("M[Int] with M[X]{def m: Any} forSome { type X }", T.baseTypeSeq.rawElem(0).toString)

    // This is the entry for M. The raw entry is an existential over a RefinedType which encodes a lazily computed base type
    assertEquals(T.baseTypeSeq.typeSymbol(1), MClass)
    assertEquals("M[X] with M[Int] forSome { type X }", T.baseTypeSeq.rawElem(1).toString)
    // calling `apply` merges the prefix/args of the elements ot the RefinedType and rewraps in the existential
    assertEquals("M[_1] forSome { type X; type _1 >: X with Int }", T.baseTypeSeq.apply(1).toString)
  }

  @Test
  def testExistentialMerge(): Unit = {
    val ts = typeOf[Set[Any]] :: typeOf[Set[X] forSome { type X <: Y; type Y <: Int}] :: Nil
    def merge(ts: List[Type]) = mergePrefixAndArgs(ts, Variance.Contravariant, lubDepth(ts))
    val merged1 = merge(ts)
    val merged2 = merge(ts.reverse)
    assert(ts.forall(_ <:< merged1)) // use to fail before fix to mergePrefixAndArgs for existentials
    assert(ts.forall(_ <:< merged2))
    assert(merged1 =:= merged2)
  }



  class Foo[A]
  class Bar[+T, A]
  class Baz {
    def f[F[_]] = ()
    def g[G[_, _]] = ()
  }

  var storedXsource: ScalaVersion = null
  @Before
  def storeXsource: Unit = {
    storedXsource = settings.source.value
  }
  @After
  def restoreXsource: Unit = {
    settings.source.value = storedXsource
  }

  @Test
  def testHigherKindedTypeVarUnification(): Unit = {
    import rootMirror.EmptyPackageClass
    import Flags._

    val FooTpe = typeOf[Foo[Int]] match {
      case TypeRef(pre, sym, _) =>
        sym.typeParams // doing it for the side effect
        TypeRef(pre, sym, Nil)
    }
    val BarTpe = typeOf[Bar[Int, Int]] match {
      case TypeRef(pre, sym, _) =>
        sym.typeParams // doing it for the side effect
        TypeRef(pre, sym, Nil)
    }

    // apply Foo to type arugment A
    def Foo(A: Type) = FooTpe match {
      case TypeRef(pre, sym, Nil) => TypeRef(pre, sym, A :: Nil)
    }

    // apply Bar to type arguments A, B
    def Bar(A: Type, B: Type) = BarTpe match {
      case TypeRef(pre, sym, Nil) => TypeRef(pre, sym, A :: B :: Nil)
    }

    val F0 = typeOf[Baz].member(TermName("f")).typeSignature.typeParams.head
    val G0 = typeOf[Baz].member(TermName("g")).typeSignature.typeParams.head

    // since TypeVars are mutable, we will be creating fresh ones
    def F() = TypeVar(F0)
    def G() = TypeVar(G0)

    def polyType(f: TypeVar => Type, flags: Long = 0L): Type = {
      val A = EmptyPackageClass.newTypeParameter(newTypeName("A"), newFlags = flags)
      A.setInfo(TypeBounds.empty)
      val A_ = TypeVar(A)
      PolyType(A :: Nil, f(A_))
    }

    def coPolyType(f: TypeVar => Type): Type =
      polyType(f, COVARIANT)

    def polyType2(f: (TypeVar, TypeVar) => Type): Type = {
      val A = EmptyPackageClass.newTypeParameter(newTypeName("A"))
      val B = EmptyPackageClass.newTypeParameter(newTypeName("B"))
      A.setInfo(TypeBounds.empty)
      B.setInfo(TypeBounds.empty)
      val A_ = TypeVar(A)
      val B_ = TypeVar(B)
      PolyType(A :: B :: Nil, f(A_, B_))
    }

    val Any = typeOf[Any]
    val Int = typeOf[Int]

    settings.source.value = ScalaVersion("2.13")

    // test that ?F unifies with Foo
    assert(F() <:< FooTpe)
    assert(FooTpe <:< F())
    assert(F() =:= FooTpe)
    assert(FooTpe =:= F)

    // test that ?F unifies with [A]Foo[A]
    assert(F() <:< polyType(A => Foo(A)))
    assert(polyType(A => Foo(A)) <:< F())
    assert(F() =:= polyType(A => Foo(A)))
    assert(polyType(A => Foo(A)) =:= F())

    // test that ?F unifies with [A]Bar[Int, A]
    assert(F() <:< polyType(A => Bar(Int, A)))
    assert(polyType(A => Bar(Int, A)) <:< F())
    assert(F() =:= polyType(A => Bar(Int, A)))
    assert(polyType(A => Bar(Int, A)) =:= F())

    // test that ?F unifies with [A]Bar[A, Int]
    assert(F() <:< polyType(A => Bar(A, Int)))
    assert(polyType(A => Bar(A, Int)) <:< F())
    assert(F() =:= polyType(A => Bar(A, Int)))
    assert(polyType(A => Bar(A, Int)) =:= F())

    // test that ?F unifies with [+A]Bar[A, Int]
    assert(F() <:< coPolyType(A => Bar(A, Int)))
    assert(coPolyType(A => Bar(A, Int)) <:< F())
    assert(F() =:= coPolyType(A => Bar(A, Int)))
    assert(coPolyType(A => Bar(A, Int)) =:= F())

    // test that ?F unifies with [A]Foo[Foo[A]]
    assert(F() <:< polyType(A => Foo(Foo(A))))
    assert(polyType(A => Foo(Foo(A))) <:< F())
    assert(F() =:= polyType(A => Foo(Foo(A))))
    assert(polyType(A => Foo(Foo(A))) =:= F())

    // test that ?F unifies with [A]Foo[Bar[A, A]]
    assert(F() <:< polyType(A => Foo(Bar(A, A))))
    assert(polyType(A => Foo(Bar(A, A))) <:< F())
    assert(F() =:= polyType(A => Foo(Bar(A, A))))
    assert(polyType(A => Foo(Bar(A, A))) =:= F())

    // test that ?F unifies with [A]Bar[Foo[A], Foo[A]]
    assert(F() <:< polyType(A => Bar(Foo(A), Foo(A))))
    assert(polyType(A => Bar(Foo(A), Foo(A))) <:< F())
    assert(F() =:= polyType(A => Bar(Foo(A), Foo(A))))
    assert(polyType(A => Bar(Foo(A), Foo(A))) =:= F())

    // test that ?F unifies with [A]A
    assert(F() <:< polyType(A => A))
    assert(polyType(A => A) <:< F())
    assert(F() =:= polyType(A => A))
    assert(polyType(A => A) =:= F())

    // test that ?F unifies with [A]Int
    assert(F() <:< polyType(A => Int))
    assert(polyType(A => Int) <:< F())
    assert(F() =:= polyType(A => Int))
    assert(polyType(A => Int) =:= F())

    // test that ?F unifies with [A]Foo[Int]
    assert(F() <:< polyType(A => Foo(Int)))
    assert(polyType(A => Foo(Int)) <:< F())
    assert(F() =:= polyType(A => Foo(Int)))
    assert(polyType(A => Foo(Int)) =:= F())

    // test that ?G unifies with Bar
    assert(G() <:< BarTpe)
    assert(BarTpe <:< G())
    assert(G() =:= BarTpe)
    assert(BarTpe =:= G())

    // test that ?G unifies with [A, B]Bar[A, B]
    assert(G() <:< polyType2((A, B) => Bar(A, B)))
    assert(polyType2((A, B) => Bar(A, B)) <:< G())
    assert(G() =:= polyType2((A, B) => Bar(A, B)))
    assert(polyType2((A, B) => Bar(A, B)) =:= G())

    // test that ?G unifies with [A, B]Bar[B, A]
    assert(G() <:< polyType2((A, B) => Bar(B, A)))
    assert(polyType2((B, A) => Bar(A, B)) <:< G())
    assert(G() =:= polyType2((A, B) => Bar(B, A)))
    assert(polyType2((B, A) => Bar(A, B)) =:= G())

    // test that ?G unifies with [A, B]Bar[Bar[B, A], A]
    assert(G() <:< polyType2((A, B) => Bar(Bar(B, A), A)))
    assert(polyType2((A, B) => Bar(Bar(B, A), A)) <:< G())
    assert(G() =:= polyType2((A, B) => Bar(Bar(B, A), A)))
    assert(polyType2((A, B) => Bar(Bar(B, A), A)) =:= G())

    // test that [A]Bar[Int, A] <:< ?F <:< [A]Bar[Any, A]
    F() match { case _F =>
      assert(polyType(A => Bar(Int, A)) <:< _F && _F <:< polyType(A => Bar(Any, A)))
    }
  }

  @Test
  def testAnyNothing(): Unit = {
    object Foo { val a: Any = 23 ; val n: Nothing = ??? }
    val aSym = typeOf[Foo.type].member(TermName("a"))
    val nSym = typeOf[Foo.type].member(TermName("n"))

    assert(typeIsAnyOrJavaObject(AnyTpe))
    assert(typeIsNothing(NothingTpe))
    assert(!typeIsAnyOrJavaObject(SingleType(NoPrefix, aSym)))
    assert(!typeIsNothing(SingleType(NoPrefix, nSym)))
  }
}
