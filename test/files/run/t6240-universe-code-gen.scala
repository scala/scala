
import scala.jdk.CollectionConverters._
import scala.tools.partest.nest.FileManager._

import java.io.File
import java.nio.file.Files

object Test extends App {
  val cm = reflect.runtime.currentMirror
  val u = cm.universe
  import u._

  val JavaUniverseTpe = typeOf[reflect.runtime.JavaUniverse]
  val DefinitionsModule = JavaUniverseTpe.member(TermName("definitions"))

  def forceCode(prefix: String, tp: Type): String = {
    def isLazyAccessorOrObject(sym: Symbol) = (
          (sym.isMethod && sym.asMethod.isLazy)
       || sym.isModule
    )
    val forceables = tp.members.sorted.filter(isLazyAccessorOrObject)
    forceables.map {
      sym =>
        val path = s"$prefix.${sym.name}"
        "    " + (
          if (sym.isPrivate || sym.isProtected) s"// inaccessible: $path"
          else path
        )
    }.mkString("\n")
  }

  val code =
    s"""|/*
        | * Scala (https://www.scala-lang.org)
        | *
        | * Copyright EPFL and Lightbend, Inc. dba Akka
        | *
        | * Licensed under Apache License 2.0
        | * (http://www.apache.org/licenses/LICENSE-2.0).
        | *
        | * See the NOTICE file distributed with this work for
        | * additional information regarding copyright ownership.
        | */
        |
        |// Generated Code, validated by run/t6240-universe-code-gen.scala
        |package scala.reflect
        |package runtime
        |
        |import scala.annotation.nowarn
        |
        |@nowarn("cat=deprecation&origin=scala\\\\.reflect\\\\.internal\\\\.Internals\\\\.compat")
        |@nowarn("cat=deprecation&origin=scala\\\\.reflect\\\\.internal\\\\.Trees\\\\.emptyValDef")
        |trait JavaUniverseForce { self: runtime.JavaUniverse  =>
        |  def force(): Unit = {
        |    Literal(Constant(42)).duplicate
        |    nme.flattenedName(NoSymbol, nme.NO_NAME)
        |    nme.raw
        |    WeakTypeTag
        |    TypeTag
        |    TypeTag.Byte.tpe
        |    TypeTag.Short.tpe
        |    TypeTag.Char.tpe
        |    TypeTag.Int.tpe
        |    TypeTag.Long.tpe
        |    TypeTag.Float.tpe
        |    TypeTag.Double.tpe
        |    TypeTag.Boolean.tpe
        |    TypeTag.Unit.tpe
        |    TypeTag.Any.tpe
        |    TypeTag.AnyVal.tpe
        |    TypeTag.AnyRef.tpe
        |    TypeTag.Object.tpe
        |    TypeTag.Nothing.tpe
        |    TypeTag.Null.tpe
        |
        |${forceCode("this", JavaUniverseTpe)}
        |${forceCode("definitions", DefinitionsModule.info)}
        |
        |${forceCode("uncurry", typeOf[scala.reflect.internal.transform.UnCurry])}
        |${forceCode("erasure", typeOf[scala.reflect.internal.transform.Erasure])}
        |  }
        |}""".stripMargin

  val testFile = new File(sys.props("partest.test-path"))
  val actualFile = new File(testFile.getParent + "/../../../src/reflect/scala/reflect/runtime/JavaUniverseForce.scala").getCanonicalFile
  val actualLines = Files.readAllLines(actualFile.toPath).asScala.toList
  val generatedLines = code.linesIterator.toList
  if (actualLines != generatedLines) {
    val msg = s"""|${actualFile} must be updated.
                  |===========================================================
                  | DIFF:
                  |===========================================================
                  |${compareContents(actualLines, generatedLines)}
                  |===========================================================
                  | NEW CONTENTS:
                  |===========================================================
                  |${code}""".stripMargin

    assert(false, msg)
  }
}
