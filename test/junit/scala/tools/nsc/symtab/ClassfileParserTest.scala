package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith
import org.junit.runners.JUnit4
import org.openjdk.jol.info.{GraphLayout, GraphPathRecord, GraphVisitor, GraphWalker}

import scala.tools.testing.AssertUtil.assertThrows
import scala.reflect.internal.util.OffsetPosition
import scala.tools.nsc.symtab.classfile.AbstractFileReader

@RunWith(classOf[JUnit4])
class ClassfileParserTest {
  object symbolTable extends SymbolTableForUnitTesting
  import symbolTable._

  @Test def retention(): Unit = {
    definitions.StringClass
    System.gc()
    System.gc()
    System.gc()
    val before: GraphLayout = GraphLayout.parseInstance(symbolTable)
    definitions.StringClass.info
    System.gc()
    System.gc()
    System.gc()
    val after: GraphLayout = GraphLayout.parseInstance(symbolTable)
    println(after.subtract(before).toFootprint)
  }
}
