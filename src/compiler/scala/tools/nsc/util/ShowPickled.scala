/* NSC -- new Scala compiler
 * Copyright 2005-2010 LAMP/EPFL
 * @author  Martin Odersky
 */
// $Id$

package scala.tools.nsc
package util

import java.io.{File, FileInputStream, PrintStream, IOException}
import java.lang.Long.toHexString
import java.lang.Float.intBitsToFloat
import java.lang.Double.longBitsToDouble

import symtab.{ Flags, Names }
import scala.reflect.generic.{ PickleBuffer, PickleFormat }
import interpreter.ByteCode.scalaSigBytesForPath

object ShowPickled extends Names {

  import PickleFormat._

  def tag2string(tag: Int): String = tag match {
    case TERMname       => "TERMname"
    case TYPEname       => "TYPEname"
    case NONEsym        => "NONEsym"
    case TYPEsym        => "TYPEsym"
    case ALIASsym       => "ALIASsym"
    case CLASSsym       => "CLASSsym"
    case MODULEsym      => "MODULEsym"
    case VALsym         => "VALsym"
    case EXTref         => "EXTref"
    case EXTMODCLASSref => "EXTMODCLASSref"
    case NOtpe          => "NOtpe"
    case NOPREFIXtpe    => "NOPREFIXtpe"
    case THIStpe        => "THIStpe"
    case SINGLEtpe      => "SINGLEtpe"
    case CONSTANTtpe    => "CONSTANTtpe"
    case TYPEREFtpe     => "TYPEREFtpe"
    case TYPEBOUNDStpe  => "TYPEBOUNDStpe"
    case REFINEDtpe     => "REFINEDtpe"
    case CLASSINFOtpe   => "CLASSINFOtpe"
    case METHODtpe      => "METHODtpe"
    case POLYtpe        => "POLYtpe"
    case IMPLICITMETHODtpe => "IMPLICITMETHODtpe"
    case SUPERtpe       => "SUPERtpe"
    case LITERALunit    => "LITERALunit"
    case LITERALboolean => "LITERALboolean"
    case LITERALbyte    => "LITERALbyte"
    case LITERALshort   => "LITERALshort"
    case LITERALchar    => "LITERALchar"
    case LITERALint     => "LITERALint"
    case LITERALlong    => "LITERALlong"
    case LITERALfloat   => "LITERALfloat"
    case LITERALdouble  => "LITERALdouble"
    case LITERALstring  => "LITERALstring"
    case LITERALnull    => "LITERALnull"
    case LITERALclass   => "LITERALclass"
    case LITERALenum    => "LITERALenum"
    case SYMANNOT       => "SYMANNOT"
    case CHILDREN       => "CHILDREN"
    case ANNOTATEDtpe   => "ANNOTATEDtpe"
    case ANNOTINFO      => "ANNOTINFO"
    case ANNOTARGARRAY  => "ANNOTARGARRAY"
    case DEBRUIJNINDEXtpe => "DEBRUIJNINDEXtpe"
    case EXISTENTIALtpe => "EXISTENTIALtpe"
    case TREE           => "TREE"
    case MODIFIERS      => "MODIFIERS"

    case _ => "***BAD TAG***(" + tag + ")"
  }

  def printFile(buf: PickleBuffer, out: PrintStream) {
    out.println("Version " + buf.readNat() + "." + buf.readNat())
    val index = buf.createIndex

    def printNameRef() {
      val x = buf.readNat()
      val savedIndex = buf.readIndex
      buf.readIndex = index(x)
      val tag = buf.readByte()
      val len = buf.readNat()
      out.print(" " + x + "(" + newTermName(buf.bytes, buf.readIndex, len) + ")")
      buf.readIndex = savedIndex
    }

    def printNat() = out.print(" " + buf.readNat())
    def printSymbolRef() = printNat()
    def printTypeRef() = printNat()
    def printConstantRef() = printNat()
    def printAnnotInfoRef() = printNat()
    def printConstAnnotArgRef() = printNat()
    def printAnnotArgRef() = printNat()

    def printSymInfo() {
      printNameRef()
      printSymbolRef()
      val pflags = buf.readLongNat()
      out.print(" " + toHexString(pflags) +
                "[" + Flags.flagsToString(Flags.pickledToRawFlags(pflags)) + "] ")
      printTypeRef()
    }

    /** Note: the entries which require some semantic analysis to be correctly
     *  interpreted are for the most part going to tell you the wrong thing.
     *  It's not so easy to duplicate the logic applied in the UnPickler.
     */
    def printEntry(i: Int) {
      buf.readIndex = index(i)
      out.print(i + "," + buf.readIndex + ": ")
      val tag = buf.readByte()
      out.print(tag2string(tag))
      val len = buf.readNat()
      val end = len + buf.readIndex
      out.print(" " + len + ":")
      tag match {
        case TERMname =>
          out.print(" ")
          out.print(newTermName(buf.bytes, buf.readIndex, len).toString())
          buf.readIndex = end
        case TYPEname =>
          out.print(" ")
          out.print(newTypeName(buf.bytes, buf.readIndex, len))
          buf.readIndex = end
        case TYPEsym | ALIASsym | CLASSsym | MODULEsym | VALsym =>
          printSymInfo()
          if (tag == CLASSsym && (buf.readIndex < end)) printTypeRef()
        case EXTref | EXTMODCLASSref =>
          printNameRef()
          if (buf.readIndex < end) { printSymbolRef() }
        case THIStpe =>
          printSymbolRef()
        case SINGLEtpe =>
          printTypeRef(); printSymbolRef()
        case CONSTANTtpe =>
          printTypeRef(); printConstantRef()
        case TYPEREFtpe =>
          printTypeRef(); printSymbolRef(); buf.until(end, printTypeRef)
        case TYPEBOUNDStpe =>
          printTypeRef(); printTypeRef()
        case REFINEDtpe =>
          printSymbolRef(); buf.until(end, printTypeRef)
        case CLASSINFOtpe =>
          printSymbolRef(); buf.until(end, printTypeRef)
        case METHODtpe | IMPLICITMETHODtpe =>
          printTypeRef(); buf.until(end, printTypeRef)
        case POLYtpe =>
          printTypeRef(); buf.until(end, printSymbolRef)
        case LITERALboolean =>
          out.print(if (buf.readLong(len) == 0L) " false" else " true")
        case LITERALbyte    =>
          out.print(" " + buf.readLong(len).toByte)
        case LITERALshort   =>
          out.print(" " + buf.readLong(len).toShort)
        case LITERALchar    =>
          out.print(" " + buf.readLong(len).toChar)
        case LITERALint     =>
          out.print(" " + buf.readLong(len).toInt)
        case LITERALlong    =>
          out.print(" " + buf.readLong(len))
        case LITERALfloat   =>
          out.print(" " + intBitsToFloat(buf.readLong(len).toInt))
        case LITERALdouble  =>
          out.print(" " + longBitsToDouble(buf.readLong(len)))
        case LITERALstring  =>
          printNameRef()
        case LITERALenum    =>
          printSymbolRef()
        case LITERALnull    =>
          out.print(" <null>")
        case LITERALclass   =>
          printTypeRef()
        case CHILDREN       =>
          printSymbolRef(); buf.until(end, printSymbolRef)
        case SYMANNOT       =>
          printSymbolRef(); printTypeRef(); buf.until(end, printAnnotArgRef)
        case ANNOTATEDtpe   =>
          printTypeRef(); buf.until(end, printAnnotInfoRef);
        case ANNOTINFO      =>
          printTypeRef(); buf.until(end, printAnnotArgRef)
        case ANNOTARGARRAY  =>
          buf.until(end, printConstAnnotArgRef)

        case _ =>
      }
      out.println()
      if (buf.readIndex != end) {
        out.println("BAD ENTRY END: computed = %d, actual = %d, bytes = %s".format(
          end, buf.readIndex, buf.bytes.slice(index(i), (end max buf.readIndex)).mkString(", ")
        ))
      }
    }

    for (i <- 0 until index.length) printEntry(i)
  }

  def fromBytes(what: String, data: => Array[Byte]): Boolean = {
    try {
      val pickle = new PickleBuffer(data, 0, data.length)
      Console.println(what + ": ")
      printFile(pickle, Console.out)
      true
    }
    catch {
      case _: Exception => false
    }
  }

  def fromFile(path: String) = fromBytes(path, io.File(path).toByteArray)
  def fromName(name: String) = fromBytes(name, scalaSigBytesForPath(name) getOrElse Array())

  def main(args: Array[String]) {
    args foreach { arg =>
      val res = fromFile(arg) || fromName(arg)
      if (!res)
        Console.println("Cannot read " + arg)
    }
  }
}
