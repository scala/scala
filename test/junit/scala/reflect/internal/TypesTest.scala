package scala.reflect.internal

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
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
    // cause of SI-8611. See `narrowUniquely` in `Logic` for the workaround.
    assert(boolWithString1narrow1 =:= boolWithString1narrow2)
    val uniquelyNarrowed1 = refinedType(boolWithString1narrow1 :: Nil, NoSymbol)
    val uniquelyNarrowed2 = refinedType(boolWithString1narrow2 :: Nil, NoSymbol)
    assert(uniquelyNarrowed1 =:= uniquelyNarrowed2)
  }

  @Test
  def testSameTypesLub(): Unit = {
    settings.YliteralTypes.value = true

    def testSameType(tpe: Type, num: Int = 5) = assert(lub(List.fill(num)(tpe)) =:= tpe)

    testSameType(IntTpe)
    testSameType(StringTpe)
    testSameType(typeOf[Class[String]])
    testSameType(LiteralType(Constant(1)))
    testSameType(LiteralType(Constant("test")))
  }

  @Test
  def testTypesLub(): Unit = {
    settings.YliteralTypes.value = true

    val interestingCombos: Map[Type, List[List[Type]]] = Map(
      IntTpe -> List(
        List(LiteralType(Constant(0)), LiteralType(Constant(1))),
        List(LiteralType(Constant(0)), IntTpe),
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
      checks.foreach(check => assert(lub(check) =:= result))
    }
  }
}
