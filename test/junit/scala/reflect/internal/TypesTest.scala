package scala.reflect.internal

import org.junit.Assert._
import org.junit.{After, Assert, Before, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable
import scala.tools.nsc.symtab.SymbolTableForUnitTesting

@RunWith(classOf[JUnit4])
class TypesTest {

  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._, definitions._

  var storedYpartialUnification = false

  @Before
  def storeYpartialUnification: Unit = {
    storedYpartialUnification = settings.YpartialUnification
  }

  @After
  def restoreYpartialUnification: Unit = {
    settings.YpartialUnification.value = storedYpartialUnification
  }

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
  class Bar[A, B]
  class Baz {
    def f[F[_]] = ()
    def g[G[_, _]] = ()
  }

    val FooTpe = typeOf[Foo[Int]] match {
      case TypeRef(pre, sym, _) =>
        sym.typeParams // doing it for the side effect of initializing type params
        TypeRef(pre, sym, Nil)
    }
    val BarTpe = typeOf[Bar[Int, Int]] match {
      case TypeRef(pre, sym, _) =>
        sym.typeParams // doing it for the side effect of initializing type params
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

    // Creates a type variable F[_], not applied to any type arguments.
    // Since TypeVars are mutable, we will be creating fresh ones.
    def F() = TypeVar(F0)

    // Creates a type variable G[_, _], not applied to any type arguments.
    // Since TypeVars are mutable, we will be creating fresh ones.
    def G() = TypeVar(G0)

    def polyType(f: TypeVar => Type): Type = {
      val A = rootMirror.EmptyPackageClass.newTypeParameter(newTypeName("A"))
      A.setInfo(TypeBounds.empty)
      val A_ = TypeVar(A)
      PolyType(A :: Nil, f(A_))
    }

    val Any = typeOf[Any]
    val Int = typeOf[Int]

  @Test
  def testHigherKindedTypeVarUnification1(): Unit = {
    settings.YpartialUnification.value = true

    // test that ?G doesn't unify with [A]Bar[A, A]
    // (wrong number of type parameters (2 vs 1)
    assertFalse(G() <:< polyType(A => Bar(A, A)))
    assertFalse(polyType(A => Bar(A, A)) <:< G())
    // The following fail, but shouldn't.
    // Probably a bug in TypeVar#registerTypeEquality: not checking the number of type parameters.
    assertFalse(G() =:= polyType(A => Bar(A, A))) // FAILS!
    assertFalse(polyType(A => Bar(A, A)) =:= G()) // FAILS!
  }

  @Test
  def testHigherKindedTypeVarUnification2(): Unit = {
    settings.YpartialUnification.value = true

    // Test that both
    //   Foo <:< ?F
    //   ?F <:< [A]Bar[Int, A]
    // but not at the same time! That, by transitivity, would mean
    //   Foo <:< [A]Bar[Int, A]
    // which is wrong.

    assert(FooTpe <:< F())
    assert(F() <:< polyType(A => Bar(Int, A)))

    F() match { case _F =>
      assertFalse(FooTpe <:< _F && _F <:< polyType(A => Bar(Int, A))) // FAILS!
    }
  }

}
