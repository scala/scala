/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import scala.collection.mutable._;


class ScalaAttribute(in: ByteArrayReader) {

    val table = readTable;

    def readTable: Array[AttribEntry] = {
        val res = new Array[AttribEntry](in.nextNat);
        var i = 0;
        while (i < res.length) {
            res(i) = readTableEntry;
            i = i + 1;
        }
        res
    }

    def readTableEntry: AttribEntry = {
        val tag = in.nextByte;
        val len = in.nextNat;
        //Console.println("" + tag + ": " + len);
        val end = in.bp + len;
        tag match {
            case 1 /* TERMname */ =>
                TermName(in.nextUTF8(len))
            case 2 /* TYPEname */ =>
                TypeName(in.nextUTF8(len))
            case 3 /* NONEsym */ =>
                NoneSym()
            case 4 /* TYPEsym */ =>
                TypeSym(readSymInfo, in.nextNat)
            case 5 /* ALIASsym */ =>
                AliasSym(readSymInfo, in.nextNat)
            case 6 /* CLASSsym */ =>
                ClassSym(readSymInfo, in.nextNat, in.nextNat)
            case 7 /* VALsym */ =>
                ValSym(readSymInfo, if (in.bp < end) in.nextNat else -1)
            case 8 /* EXTref */ =>
                ExtRef(false, in.nextNat, if (in.bp < end) in.nextNat else -1)
            case 9 /* EXTMODCLASSref */ =>
                ExtRef(true, in.nextNat, if (in.bp < end) in.nextNat else -1)
            case 10 /* NOtpe */ =>
                NoneType()
            case 11 /* THIStpe */ =>
                SelfType(in.nextNat)
            case 12 /* SINGLEtpe */ =>
                SingleType(in.nextNat, in.nextNat)
            case 13 /* TYPEREFtpe */ =>
                TypeReference(in.nextNat, in.nextNat, readRefs(end))
            case 14 /* COMPOUNDtpe */ =>
                CompoundTypeRef(in.nextNat, readRefs(end))
            case 15 /* METHODtpe */ =>
                MethodTypeRef(in.nextNat, readRefs(end))
            case 16 /* POLYtpe */ =>
                PolyTypeRef(in.nextNat, readRefs(end))
            case 17 /* OVERLOADEDtpe */ =>
                OverloadedTypeRef(readRefs(end))
            case 20 /* FLAGGEDtpe */ =>
                FlaggedType(in.nextNat, in.nextNat)
        }
    }

    def readSymInfo: SymbolInfo =
        SymbolInfo(in.nextNat, in.nextNat, in.nextNat, in.nextNat);

    def readRefs(end: Int): List[Int] = {
        var res = new Buffer[Int];
        while (in.bp < end)
            res += in.nextNat;
        res.toList
    }

    class AttribEntry;
    case class TermName(name: String) extends AttribEntry;
    case class TypeName(name: String) extends AttribEntry;
    case class NoneSym() extends AttribEntry;
    case class TypeSym(info: SymbolInfo, lobound: Int) extends AttribEntry;
    case class AliasSym(info: SymbolInfo, constr: Int) extends AttribEntry;
    case class ClassSym(info: SymbolInfo, thistpe: Int, constr: Int) extends AttribEntry;
    case class ValSym(info: SymbolInfo, classsym: Int) extends AttribEntry;
    case class ExtRef(mod: Boolean, name: Int, owner: Int) extends AttribEntry;
    case class NoneType() extends AttribEntry;
    case class SelfType(sym: Int) extends AttribEntry;
    case class SingleType(typeref: Int, sym: Int) extends AttribEntry;
    case class TypeReference(typeref: Int, sym: Int, args: List[Int]) extends AttribEntry;
    case class CompoundTypeRef(sym: Int, components: List[Int]) extends AttribEntry;
    case class MethodTypeRef(res: Int, args: List[Int]) extends AttribEntry;
    case class PolyTypeRef(tpe: Int, sym: List[Int]) extends AttribEntry;
    case class OverloadedTypeRef(symandtpe: List[Int]) extends AttribEntry;
    case class FlaggedType(flags: Int, tpe: Int) extends AttribEntry;

    case class SymbolInfo(name: Int, owner: Int, flags: Int, info: Int);
}
