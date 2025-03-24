/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor, _}

import scala.jdk.CollectionConverters._
import scala.reflect.internal.pickling.ByteCodecs
import scala.reflect.io.RootPath
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.{ClassReader, ClassWriter, Opcodes}

object PickleExtractor {

  def main(args: Array[String]): Unit = {
    args.toList match {
      case input :: output :: Nil =>
        process(Paths.get(input), Paths.get(output))
      case _ =>
    }
  }
  def process(input: Path, output: Path): Unit = {
    val inputPath = RootPath(input, writable = false)
    val outputPath = RootPath(output, writable = true)
    try {
      val root = inputPath.root
      Files.createDirectories(outputPath.root)
      val visitor = new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (dir != root) {
            val outputDir = outputPath.root.resolve(root.relativize(dir).toString)
            Files.createDirectories(outputDir)
          }
          FileVisitResult.CONTINUE
        }
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (file.getFileName.toString.endsWith(".class")) {
            try {
              stripClassFile(Files.readAllBytes(file)) match {
                case Class(out) =>
                  Files.write(outputPath.root.resolve(root.relativize(file).toString), out)
                case Pickle(out) =>
                  Files.write(outputPath.root.resolve(root.relativize(file).toString.replaceAll(".class$", ".sig")), out)
                case Skip =>
              }
            } catch {
              case ex: RuntimeException =>
                throw new RuntimeException("While parsing: " + file +  " in " + inputPath
                  , ex)

            }
          }
          FileVisitResult.CONTINUE
        }
      }
      Files.walkFileTree(root, visitor)
    } finally {
      inputPath.close()
      outputPath.close()
    }
  }

  def stripClassFile(classfile: Array[Byte]): OutputFile = {
    val input = new ClassNode()
    new ClassReader(classfile).accept(input, ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES | ClassReader.SKIP_CODE)
    var output = new ClassNode()
    output.name = input.name
    output.access = input.access
    output.version = input.version

    var foundScalaSig = false

    def isScalaAnnotation(desc: String) = (desc == "Lscala/reflect/ScalaSignature;" || desc == "Lscala/reflect/ScalaLongSignature;") && {
      foundScalaSig = true

      true
    }

    var pickleData: Array[Byte] = null
    if (input.visibleAnnotations != null) {
      input.visibleAnnotations.asScala.foreach { node =>
        if (node.desc == "Lscala/reflect/ScalaSignature;") {
          val Array("bytes", data: String) = node.values.toArray(): @unchecked
          val bytes = data.getBytes(UTF_8)
          val len = ByteCodecs.decode(bytes)
          pickleData = bytes.take(len)
        } else if (node.desc == "Lscala/reflect/ScalaLongSignature;") {
          val Array("bytes", data: java.util.Collection[String @unchecked]) = node.values.toArray(): @unchecked
          val encoded = data.asScala.toArray.flatMap(_.getBytes(UTF_8))
          val len = ByteCodecs.decode(encoded)
          pickleData = encoded.take(len)
        }
      }
      output.visibleAnnotations = input.visibleAnnotations.asScala.filter(node => isScalaAnnotation(node.desc) && {
        true
      }).asJava
    }
    var foundScalaAttr = false
    if (input.attrs != null) {
      output.attrs = input.attrs.asScala.filter(attr => (attr.`type` == "Scala" || attr.`type` == "ScalaSig") && {
        foundScalaAttr = true;
        true
      }).asJava
    }
    val writer = new ClassWriter(Opcodes.ASM7)
    val isScalaRaw = foundScalaAttr && !foundScalaSig
    if (isScalaRaw) Skip
    else {
      if (pickleData == null) {
        output = input
        output.accept(writer)
        Class(writer.toByteArray)
      } else {
        output.accept(writer)
        Pickle(pickleData)
      }
    }
  }

  sealed abstract class OutputFile

  case object Skip extends OutputFile

  case class Class(content: Array[Byte]) extends OutputFile

  case class Pickle(content: Array[Byte]) extends OutputFile

}
