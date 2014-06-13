/*
 * Copyright (c) 2014 Contributor. All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Scala License which accompanies this distribution, and
 * is available at http://www.scala-lang.org/license.html
 */
package scala.tools.nsc
package symtab

import org.junit.Assert._
import org.junit.Test
import org.junit.runner.RunWith

import org.junit.runners.JUnit4
import scala.tools.nsc.settings.ClassPathImplementationType

@RunWith(classOf[JUnit4])
class SymbolTableTest {
  object symbolTable extends SymbolTableForUnitTesting

  @Test
  def initDefinitions = {
    symbolTable.settings.debug.value = true
    symbolTable.definitions.init()
  }

  @Test
  def basicSubTypeCheck = {
    var i = 0
    while (i < 200) {
      object symbolTable extends SymbolTableForUnitTesting
      symbolTable.settings.YclasspathImpl.value = ClassPathImplementationType.Flat
      symbolTable.definitions.init()
      val listClassTpe = symbolTable.definitions.ListClass.tpe
      val seqClassTpe = symbolTable.definitions.SeqClass.tpe
      assertTrue("List should be subclass of Seq", listClassTpe <:< seqClassTpe)
      i += 1
    }
  }

  /**
   * Demonstrates how one can create symbols and type completely
   * from scratch and perform sub type check.
   */
  @Test
  def customClassesSubTypeCheck: Unit = {
    import symbolTable._
    symbolTable.definitions.init()
    val rootClass = symbolTable.rootMirror.RootClass
    val fooSymbol = rootClass.newClassSymbol("Foo": TypeName, NoPosition, 0)
    val fooType = new ClassInfoType(Nil, EmptyScope, fooSymbol)
    fooSymbol.info = fooType
    val barSymbol = rootClass.newClassSymbol("Bar": TypeName, NoPosition, 0)
    val fooTypeRef = TypeRef(fooSymbol.owner.tpe, fooSymbol, Nil)
    val barType = new ClassInfoType(List(fooTypeRef), EmptyScope, barSymbol)
    barSymbol.info = barType
    assertTrue("Bar should be subclass of Foo", barSymbol.tpe <:< fooSymbol.tpe)
    assertFalse("Foo should be a superclass of Foo", fooSymbol.tpe <:< barSymbol.tpe)
  }

}
