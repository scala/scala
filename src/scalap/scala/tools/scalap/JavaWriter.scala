/*
 * Scala classfile decoder (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.scalap

import java.io._
import scala.reflect.NameTransformer

class JavaWriter(classfile: Classfile, writer: Writer) extends CodeWriter(writer) {

  val cf = classfile

  def flagsToStr(clazz: Boolean, flags: Int): String = {
    val buffer = new StringBuffer()
    if (((flags & 0x0007) == 0) &&
      ((flags & 0x0002) != 0))
      buffer.append("private ")
    if ((flags & 0x0004) != 0)
      buffer.append("protected ")
    if ((flags & 0x0010) != 0)
      buffer.append("final ")
    if ((flags & 0x0400) != 0)
      if (clazz) buffer.append("abstract ")
          else buffer.append("/*deferred*/ ")
    buffer.toString()
  }

  def nameToClass(str: String): String = {
    val res = NameTransformer.decode(str.replace('/', '.'))
    if (res == "java.lang.Object") "scala.Any" else res
  }

  def nameToClass0(str: String) = {
    val res = NameTransformer.decode(str.replace('/', '.'))
    if (res == "java.lang.Object") "scala.AnyRef" else res
  }

  def nameToSimpleClass(str: String) =
    NameTransformer.decode(str.substring(str.lastIndexOf('/') + 1))

  def nameToPackage(str: String) = {
    val inx = str.lastIndexOf('/')
    val name = if (inx == -1) str else str.substring(0, inx).replace('/', '.')
    NameTransformer.decode(name)
  }

  def sigToType(str: String): String =
    sigToType(str, 0)._1

  def sigToType(str: String, i: Int): (String, Int) = str.charAt(i) match {
    case 'B' => ("scala.Byte", i + 1)
    case 'C' => ("scala.Char", i + 1)
    case 'D' => ("scala.Double", i + 1)
    case 'F' => ("scala.Float", i + 1)
    case 'I' => ("scala.Int", i + 1)
    case 'J' => ("scala.Long", i + 1)
    case 'S' => ("scala.Short", i + 1)
    case 'V' => ("scala.Unit", i + 1)
    case 'Z' => ("scala.Boolean", i + 1)
    case 'L' =>
      val j = str.indexOf(';', i)
      (nameToClass(str.substring(i + 1, j)), j + 1)
    case '[' =>
      val (tpe, j) = sigToType(str, i + 1)
      ("scala.Array[" + tpe + "]", j)
    case '(' =>
      val (tpe, j) = sigToType0(str, i + 1)
      ("(" + tpe, j)
    case ')' =>
      val (tpe, j) = sigToType(str, i + 1)
      ("): " + tpe, j)
  }

  def sigToType0(str: String, i: Int): (String, Int) =
    if (str.charAt(i) == ')')
      sigToType(str, i)
    else {
      val (tpe, j) = sigToType(str, i)
      if (str.charAt(j) == ')') {
        val (rest, k) = sigToType(str, j)
        (tpe + rest, k)
      } else {
        val (rest, k) = sigToType0(str, j)
        (tpe + ", " + rest, k)
      }
    }

  def getName(n: Int): String = {
    import cf.pool._

    cf.pool(n) match {
      case UTF8(str) => str
      case StringConst(m) => getName(m)
      case ClassRef(m) => getName(m)
      case _ => "<error>"
    }
  }

  def getClassName(n: Int): String = nameToClass(getName(n))

  def getSimpleClassName(n: Int): String = nameToSimpleClass(getName(n))

  def getPackage(n: Int): String = nameToPackage(getName(n))

  def getType(n: Int): String = sigToType(getName(n))

  def isStatic(flags: Int) = (flags & 0x0008) != 0

  def isInterface(flags: Int) = (flags & 0x0200) != 0

  def isConstr(name: String) = (name == "<init>")

  def printField(flags: Int, name: Int, tpe: Int, attribs: List[cf.Attribute]): Unit = {
    print(flagsToStr(clazz = false, flags))
    if ((flags & 0x0010) != 0)
      print("val " + NameTransformer.decode(getName(name)))
    else
      print("final var " + NameTransformer.decode(getName(name)))
    print(": " + getType(tpe) + ";").newline
  }

  def printMethod(flags: Int, name: Int, tpe: Int, attribs: List[cf.Attribute]): Unit = {
    if (getName(name) == "<init>")
    print(flagsToStr(clazz = false, flags))
    if (getName(name) == "<init>") {
      print("def this" + getType(tpe) + ";").newline
    }
    else {
      print("def " + NameTransformer.decode(getName(name)))
      print(getType(tpe) + ";").newline
    }
    attribs find {
      case cf.Attribute(name, _) => getName(name) == "Exceptions"
    } match {
      case Some(cf.Attribute(_, data)) =>
        val n = ((data(0) & 0xff) << 8) + (data(1) & 0xff)
        indent.print("throws ")
        for (i <- Iterator.range(0, n) map {x => 2 * (x + 1)}) {
          val inx = ((data(i) & 0xff) << 8) + (data(i+1) & 0xff)
          if (i > 2) print(", ")
          print(getClassName(inx).trim())
        }
        undent.newline
      case None =>
    }
  }

  def printClassHeader(): Unit = {
    if (isInterface(cf.flags)) {
      print("trait " + getSimpleClassName(cf.classname))
    } else {
      print("class " + getSimpleClassName(cf.classname))
      if (cf.pool(cf.superclass) != null)
        print(" extends " + nameToClass0(getName(cf.superclass)))
    }
    cf.interfaces foreach {
      n => print(" with " + getClassName(n))
    }
  }

  def printClass(): Unit = {
    val pck = getPackage(cf.classname)
    if (pck.length() > 0)
      println("package " + pck + ";")
    print(flagsToStr(clazz = true, cf.flags))
    cf.attribs find {
      case cf.Attribute(name, _) => getName(name) == "JacoMeta"
    } match {
      case None =>
        printClassHeader();
      case Some(cf.Attribute(_, data)) =>
        val mp = new MetaParser(getName(
          ((data(0) & 0xff) << 8) + (data(1) & 0xff)).trim())
        mp.parse match {
          case None => printClassHeader();
          case Some(str) =>
            if (isInterface(cf.flags))
              print("trait " + getSimpleClassName(cf.classname) + str)
            else
              print("class " + getSimpleClassName(cf.classname) + str)
        }
    }
    var statics: List[cf.Member] = Nil
    print(" {").indent.newline
    cf.fields foreach {
      case m@cf.Member(_, flags, name, tpe, attribs) =>
        if (isStatic(flags))
          statics = m :: statics
        else
          printField(flags, name, tpe, attribs)
    }
    cf.methods foreach {
      case m@cf.Member(_, flags, name, tpe, attribs) =>
        if (isStatic(flags))
          statics = m :: statics
        else
          printMethod(flags, name, tpe, attribs)
    }
    undent.print("}").newline
    if (!statics.isEmpty) {
      print("object " + getSimpleClassName(cf.classname) + " {")
      indent.newline
      statics foreach {
        case cf.Member(true, flags, name, tpe, attribs) =>
          printField(flags, name, tpe, attribs)
        case cf.Member(false, flags, name, tpe, attribs) =>
          if (getName(name) != "<clinit>")
            printMethod(flags, name, tpe, attribs)
      }
      undent.print("}").newline
    }
  }

}
