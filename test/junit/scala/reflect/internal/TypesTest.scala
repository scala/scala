package scala.reflect.internal

import org.junit.Assert._
import org.junit.{After, Assert, Before, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.annotation.StaticAnnotation
import scala.collection.mutable
import scala.tools.nsc.settings.ScalaVersion
import scala.tools.nsc.symtab.SymbolTableForUnitTesting
import language.existentials

@RunWith(classOf[JUnit4])
class TypesTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  type EmptyList[A] = Nil.type
  class ann[A] extends StaticAnnotation

  @Test
  def testRefinedTypeSI8611(): Unit = {
    def stringNarrowed = StringTpe.narrow
    assertTrue(stringNarrowed != stringNarrowed)
    assertFalse(stringNarrowed =:= stringNarrowed)

    def boolWithString = refinedType(BooleanTpe :: StringTpe :: Nil, NoSymbol)
    assertTrue(boolWithString != boolWithString)
    assertTrue(boolWithString =:= boolWithString)

    val boolWithString1 = boolWithString
    val boolWithString1narrow1 = boolWithString1.narrow
    val boolWithString1narrow2 = boolWithString1.narrow
    // Two narrowings of the same refinement end up =:=. This was the root
    // cause of scala/bug#8611. See `narrowUniquely` in `Logic` for the workaround.
    assertTrue(boolWithString1narrow1 =:= boolWithString1narrow2)
    val uniquelyNarrowed1 = refinedType(boolWithString1narrow1 :: Nil, NoSymbol)
    val uniquelyNarrowed2 = refinedType(boolWithString1narrow2 :: Nil, NoSymbol)
    assertTrue(uniquelyNarrowed1 =:= uniquelyNarrowed2)
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

    val (otherModule, otherModuleClass) = EmptyPackageClass.newModuleAndClassSymbol(TermName("Other"), NoPosition, 0L)
    val aliasSym = otherModuleClass.newTermSymbol(TermName("alias")).setInfo(tp2)
    val tp4 = singleType(TypeRef(ThisType(EmptyPackageClass), otherModuleClass, Nil), aliasSym)

    val tps = List(tp1, tp2, tp3, tp4)
    val results = mutable.Buffer[String]()
    tps.combinations(3).flatMap(_.permutations).foreach {
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
  def testNilModuleUnification(): Unit = {
    import rootMirror.RootClass
    val nil1 = singleType(ThisType(RootClass), typeOf[scala.`package`.type].member(TermName("Nil")))
    val nil2 = typeOf[scala.collection.immutable.Nil.type].underlying

    assert(nil1.isInstanceOf[UniqueSingleType], nil1.getClass)
    assert(nil2.isInstanceOf[ModuleTypeRef],    nil2.getClass)

    val tps = List(nil1, nil2)
    val results = mutable.Buffer[String]()
    tps.permutations.foreach { case List(a, b) =>
      if (!(a =:= b))
        results += s"expected a =:= b; where a=${showRaw(a)} b=${showRaw(b)}"
    }
    assertTrue(s"Mismatches:\n${results.mkString("\n")}", results.isEmpty)
  }

  @Test
  def testRefinementContains(): Unit = {
    val refinement = typeOf[{def foo: Int}]
    assertTrue(refinement.isInstanceOf[RefinedType])
    assertTrue(refinement.contains(IntClass))
    val elem0 = refinement.baseTypeSeq(0)
    assertTrue(elem0.isInstanceOf[RefinementTypeRef])
    assertTrue(elem0.contains(IntClass))
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
    val MClass = EmptyPackageClass.newClass(TypeName("M"))
    val A = MClass.newTypeParameter(TypeName("A")).setInfo(TypeBounds.empty)
    MClass.setInfo(PolyType(A :: Nil, ClassInfoType(ObjectClass.tpeHK :: Nil, newScopeWith(), MClass)))

    // (M[Int] with M[X] { def m: Any }) forSome { type X }
    val X = NoSymbol.newExistential(TypeName("X")).setInfo(TypeBounds.empty)
    val T: Type = {
      val decls = newScopeWith(MClass.newMethod(TermName("m")).setInfo(NullaryMethodType(AnyClass.tpeHK)))
      val refined = refinedType(appliedType(MClass, IntClass.tpeHK) :: appliedType(MClass, X.tpeHK) :: Nil, NoSymbol, decls, NoPosition)
      newExistentialType(X :: Nil, refined)
    }

    val RefinementClass = T.underlying.typeSymbol
    assertTrue(RefinementClass.isRefinementClass)
    TypeRef(NoPrefix, RefinementClass, Nil) match {
      case rtr : RefinementTypeRef =>
        // ContainsCollector needs to look inside the info of symbols of RefinementTypeRefs
        assertTrue(rtr.contains(X))
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
  def testDegenerateExistentialToString(): Unit = {
    SingletonClass // enter scala.Singleton
    val freeQuantifier = typeOf[Int forSome { type x }]
    val nestedSingleton = typeOf[x.type forSome { val x: x.type forSome { val x: String } }]
    assertEquals("Int", freeQuantifier.toString)
    assertEquals("x.type forSome { val x: x.type forSome { val x: String } }", nestedSingleton.toString)
  }

  @Test
  def testExistentialMerge(): Unit = {
    val ts = typeOf[Set[Any]] :: typeOf[Set[X] forSome { type X <: Y; type Y <: Int}] :: Nil
    def merge(ts: List[Type]) = mergePrefixAndArgs(ts, Variance.Contravariant, lubDepth(ts))
    val merged1 = merge(ts)
    val merged2 = merge(ts.reverse)
    assertTrue(ts.forall(_ <:< merged1)) // use to fail before fix to mergePrefixAndArgs for existentials
    assertTrue(ts.forall(_ <:< merged2))
    assertTrue(merged1 =:= merged2)
  }

  trait Enum
  object Enum {
    val x = new Enum { }
  }

  @Test
  def testSingletonWithUnderlyingRefinementToString(): Unit = {
    assertEquals("TypesTest.this.Enum.x.type", typeOf[Enum.x.type].toString)
  }

  class Foo[A]
  class Bar[+T, A]
  class Baz {
    def f[F[_]] = ()
    def g[G[_, _]] = ()
  }

  var storedXsource: ScalaVersion = null
  @Before
  def storeXsource(): Unit = {
    storedXsource = settings.source.value
  }
  @After
  def restoreXsource(): Unit = {
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

    // apply Foo to type argument A
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
    assertTrue(F() <:< FooTpe)
    assertTrue(FooTpe <:< F())
    assertTrue(F() =:= FooTpe)
    assertTrue(FooTpe =:= F())

    // test that ?F unifies with [A]Foo[A]
    assertTrue(F() <:< polyType(A => Foo(A)))
    assertTrue(polyType(A => Foo(A)) <:< F())
    assertTrue(F() =:= polyType(A => Foo(A)))
    assertTrue(polyType(A => Foo(A)) =:= F())

    // test that ?F unifies with [A]Bar[Int, A]
    assertTrue(F() <:< polyType(A => Bar(Int, A)))
    assertTrue(polyType(A => Bar(Int, A)) <:< F())
    assertTrue(F() =:= polyType(A => Bar(Int, A)))
    assertTrue(polyType(A => Bar(Int, A)) =:= F())

    // test that ?F unifies with [A]Bar[A, Int]
    assertTrue(F() <:< polyType(A => Bar(A, Int)))
    assertTrue(polyType(A => Bar(A, Int)) <:< F())
    assertTrue(F() =:= polyType(A => Bar(A, Int)))
    assertTrue(polyType(A => Bar(A, Int)) =:= F())

    // test that ?F unifies with [+A]Bar[A, Int]
    assertTrue(F() <:< coPolyType(A => Bar(A, Int)))
    assertTrue(coPolyType(A => Bar(A, Int)) <:< F())
    assertTrue(F() =:= coPolyType(A => Bar(A, Int)))
    assertTrue(coPolyType(A => Bar(A, Int)) =:= F())

    // test that ?F unifies with [A]Foo[Foo[A]]
    assertTrue(F() <:< polyType(A => Foo(Foo(A))))
    assertTrue(polyType(A => Foo(Foo(A))) <:< F())
    assertTrue(F() =:= polyType(A => Foo(Foo(A))))
    assertTrue(polyType(A => Foo(Foo(A))) =:= F())

    // test that ?F unifies with [A]Foo[Bar[A, A]]
    assertTrue(F() <:< polyType(A => Foo(Bar(A, A))))
    assertTrue(polyType(A => Foo(Bar(A, A))) <:< F())
    assertTrue(F() =:= polyType(A => Foo(Bar(A, A))))
    assertTrue(polyType(A => Foo(Bar(A, A))) =:= F())

    // test that ?F unifies with [A]Bar[Foo[A], Foo[A]]
    assertTrue(F() <:< polyType(A => Bar(Foo(A), Foo(A))))
    assertTrue(polyType(A => Bar(Foo(A), Foo(A))) <:< F())
    assertTrue(F() =:= polyType(A => Bar(Foo(A), Foo(A))))
    assertTrue(polyType(A => Bar(Foo(A), Foo(A))) =:= F())

    // test that ?F unifies with [A]A
    assertTrue(F() <:< polyType(A => A))
    assertTrue(polyType(A => A) <:< F())
    assertTrue(F() =:= polyType(A => A))
    assertTrue(polyType(A => A) =:= F())

    // test that ?F unifies with [A]Int
    assertTrue(F() <:< polyType(A => Int))
    assertTrue(polyType(A => Int) <:< F())
    assertTrue(F() =:= polyType(A => Int))
    assertTrue(polyType(A => Int) =:= F())

    // test that ?F unifies with [A]Foo[Int]
    assertTrue(F() <:< polyType(A => Foo(Int)))
    assertTrue(polyType(A => Foo(Int)) <:< F())
    assertTrue(F() =:= polyType(A => Foo(Int)))
    assertTrue(polyType(A => Foo(Int)) =:= F())

    // test that ?G unifies with Bar
    assertTrue(G() <:< BarTpe)
    assertTrue(BarTpe <:< G())
    assertTrue(G() =:= BarTpe)
    assertTrue(BarTpe =:= G())

    // test that ?G unifies with [A, B]Bar[A, B]
    assertTrue(G() <:< polyType2((A, B) => Bar(A, B)))
    assertTrue(polyType2((A, B) => Bar(A, B)) <:< G())
    assertTrue(G() =:= polyType2((A, B) => Bar(A, B)))
    assertTrue(polyType2((A, B) => Bar(A, B)) =:= G())

    // test that ?G unifies with [A, B]Bar[B, A]
    assertTrue(G() <:< polyType2((A, B) => Bar(B, A)))
    assertTrue(polyType2((B, A) => Bar(A, B)) <:< G())
    assertTrue(G() =:= polyType2((A, B) => Bar(B, A)))
    assertTrue(polyType2((B, A) => Bar(A, B)) =:= G())

    // test that ?G unifies with [A, B]Bar[Bar[B, A], A]
    assertTrue(G() <:< polyType2((A, B) => Bar(Bar(B, A), A)))
    assertTrue(polyType2((A, B) => Bar(Bar(B, A), A)) <:< G())
    assertTrue(G() =:= polyType2((A, B) => Bar(Bar(B, A), A)))
    assertTrue(polyType2((A, B) => Bar(Bar(B, A), A)) =:= G())

    // test that [A]Bar[Int, A] <:< ?F <:< [A]Bar[Any, A]
    F() match { case _F =>
      assertTrue(polyType(A => Bar(Int, A)) <:< _F && _F <:< polyType(A => Bar(Any, A)))
    }
  }

  @Test
  def testAnyNothing(): Unit = {
    object Foo { val a: Any = 23 ; val n: Nothing = ??? }
    val aSym = typeOf[Foo.type].member(TermName("a"))
    val nSym = typeOf[Foo.type].member(TermName("n"))

    assertTrue(typeIsAnyOrJavaObject(AnyTpe))
    assertTrue(typeIsNothing(NothingTpe))
    assertTrue(!typeIsAnyOrJavaObject(LiteralType(Constant(1))))
    assertTrue(!typeIsAnyOrJavaObject(SingleType(NoPrefix, aSym)))
    assertTrue(!typeIsNothing(SingleType(NoPrefix, nSym)))
  }

  @Test
  def testSameTypesLub(): Unit = {
    def testSameType(tpe: Type, num: Int = 5) = assertTrue(lub(List.fill(num)(tpe)) =:= tpe)

    testSameType(IntTpe)
    testSameType(StringTpe)
    testSameType(typeOf[Class[String]])
    testSameType(LiteralType(Constant(1)))
    testSameType(LiteralType(Constant("test")))
  }

  @Test
  def testTypesLub(): Unit = {
    val interestingCombos: Map[Type, List[List[Type]]] = Map(
      IntTpe -> List(
        List(ConstantType(Constant(0)), IntTpe),
        List(ConstantType(Constant(0)), LiteralType(Constant(1))),
        List(LiteralType(Constant(0)), ConstantType(Constant(1)))
      ),
      StringTpe -> List(
        List(LiteralType(Constant("a")), LiteralType(Constant("b"))),
        List(LiteralType(Constant("a")), StringTpe),
        List(ConstantType(Constant("a")), StringTpe),
        List(ConstantType(Constant("a")), LiteralType(Constant("b"))),
        List(ConstantType(Constant("a")), LiteralType(Constant("b")))
      ),
      LiteralType(Constant(1)) -> List(
        List(LiteralType(Constant(1)), LiteralType(Constant(1))),
        List(ConstantType(Constant(1)), LiteralType(Constant(1))),
        List(LiteralType(Constant(1)), ConstantType(Constant(1)))
      ),
      LiteralType(Constant("a")) -> List(
        List(LiteralType(Constant("a")), LiteralType(Constant("a"))),
        List(ConstantType(Constant("a")), LiteralType(Constant("a"))),
        List(LiteralType(Constant("a")), ConstantType(Constant("a")))
      ),
      AnyValTpe -> List(
        List(LiteralType(Constant(1)), IntTpe, DoubleTpe)
      ),
      typeOf[Class[String]] -> List(
        List(typeOf[Class[String]], typeOf[Class[String]])
      ),
      typeOf[Class[_ >: String <: Object]] -> List(
        List(typeOf[Class[String]], typeOf[Class[Object]])
      )
    )

    interestingCombos foreach { case (result, checks) =>
      checks.foreach(check => assertTrue(lub(check) =:= result))
    }
  }

  @Test
  def testTypeStability(): Unit = {
    assertTrue(typeOf[EmptyList[Int]].isStable)
    assertTrue(typeOf[EmptyList[T] forSome { type T }].isStable)
    assertTrue(typeOf[Nil.type forSome { type T }].isStable)
    assertTrue(typeOf[Nil.type @ann[Int]].isStable)
    assertTrue(typeOf[Nil.type @ann[T] forSome { type T }].isStable)
    assertFalse(typeOf[x.type forSome { val x: String }].isStable)
  }
}
