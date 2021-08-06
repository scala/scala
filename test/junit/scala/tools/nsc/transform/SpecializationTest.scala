package scala.tools.nsc.transform

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

import scala.tools.nsc.symtab.SymbolTableForUnitTesting

class SpecializationTest {
  object symbolTable extends SymbolTableForUnitTesting

  @Test def testHardCodedAssumptionsAboutTupleAndFunction(): Unit = {
    // The specialization phase always runs its info transform on the specialized Function and Tuple types
    // so that the later phases can see them, even with the optimization in the specialization info transform
    // that makes it a no-op after the global phase has passed specialize.
    //
    // Initially, we just called `exitingSpecialize { TupleClass.seq.map(_.info); Function.seq.map(_.info) }`
    // but this was wasteful, as it loaded the seldom used, high-arity Tuple and Function classes, some of which
    // are pretty big in bytecode!
    //
    // So we know bake the knowledge about the max arity for which specialization is used into that code.
    // This test asserts the assumption still holds.
    import symbolTable.definitions._

    for (i <- (0 to MaxFunctionArity)) {
      val cls = FunctionClass.apply(i)
      val actual = cls.typeParams.exists(_.isSpecialized)
      val expected = i <= MaxFunctionAritySpecialized
      assertEquals(expected, actual, cls.toString)
    }

    for (i <- (1 to MaxTupleArity)) {
      val cls = TupleClass.apply(i)
      val actual = cls.typeParams.exists(_.isSpecialized)
      val expected = i <= MaxTupleAritySpecialized
      assertEquals(expected, actual, cls.toString)
    }

    for (i <- (1 to MaxProductArity)) {
      val cls = ProductClass.apply(i)
      val actual = cls.typeParams.exists(_.isSpecialized)
      val expected = i <= MaxProductAritySpecialized
      assertEquals(expected, actual, cls.toString)
    }
  }
}
