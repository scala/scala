package scala.reflect.internal

import org.junit.Assert._
import org.junit.{Assert, Test}
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import scala.collection.mutable
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
}
