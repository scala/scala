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

    final val TERM_NAME = 1;
    final val TYPE_NAME = 2;
    final val NUMBER = 3;
    final val NONE_SYM = 4;
    final val TYPE_SYM = 5;
    final val ALIAS_SYM = 6;
    final val CLASS_SYM = 7;
    final val VAL_SYM = 8;
    final val EXT_REF = 9;
    final val EXTMODCLASS_REF = 10;
    final val NO_TYPE = 11;
    final val THIS_TYPE = 12;
    final val SINGLE_TYPE = 13;
    final val CONSTANT_TYPE = 14;
    final val REF_TYPE = 15;
    final val COMPOUND_TYPE = 16;
    final val METHOD_TYPE = 17;
    final val POLY_TYPE = 18;
    final val OVERLOADED_TYPE = 19;
    final val UNBOXED_TYPE = 20;
    final val UNBOXEDARRAY_TYPE = 21;
    final val FLAGGED_TYPE = 22;
    final val ERROR_TYPE = 23;
    final val UNIT_LIT = 24;
    final val BOOL_LIT = 25;
    final val BYTE_LIT = 26;
    final val SHORT_LIT = 27;
    final val CHAR_LIT = 28;
    final val INT_LIT = 29;
    final val LONG_LIT = 30;
    final val FLOAT_LIT = 31;
    final val DOUBLE_LIT = 32;
    final val STRING_LIT = 33;
    final val NULL_LIT = 34;
    final val ZERO_LIT = 35;

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
        // Console.println("tag = " + tag);
        val len = in.nextNat;
        val end = in.bp + len;
        tag match {
            case TERM_NAME =>
                TermName(in.nextUTF8(len))
            case TYPE_NAME =>
                TypeName(in.nextUTF8(len))
            case NUMBER =>
                Number(in.nextNum(len))
            case NONE_SYM =>
                NoneSym()
            case TYPE_SYM =>
                TypeSym(readSymInfo, in.nextNat)
            case ALIAS_SYM =>
                AliasSym(readSymInfo, in.nextNat)
            case CLASS_SYM =>
                ClassSym(readSymInfo, in.nextNat, in.nextNat)
            case VAL_SYM =>
                ValSym(readSymInfo, if (in.bp < end) in.nextNat else -1)
            case EXT_REF =>
                ExtRef(false, in.nextNat, if (in.bp < end) in.nextNat else -1)
            case EXTMODCLASS_REF =>
                ExtRef(true, in.nextNat, if (in.bp < end) in.nextNat else -1)
            case NO_TYPE =>
                NoneType()
            case THIS_TYPE =>
                SelfType(in.nextNat)
            case SINGLE_TYPE =>
                SingleType(in.nextNat, in.nextNat)
            case CONSTANT_TYPE =>
                ConstantTypeRef(in.nextNat, in.nextNat)
            case REF_TYPE =>
                TypeReference(in.nextNat, in.nextNat, readRefs(end))
            case COMPOUND_TYPE =>
                CompoundTypeRef(in.nextNat, readRefs(end))
            case METHOD_TYPE =>
                MethodTypeRef(in.nextNat, readRefs(end))
            case POLY_TYPE =>
                PolyTypeRef(in.nextNat, readRefs(end))
            case OVERLOADED_TYPE =>
                OverloadedTypeRef(readRefs(end))
            case FLAGGED_TYPE =>
                FlaggedType(in.nextNat, in.nextNat)
            case UNIT_LIT |
                 NULL_LIT |
                 ZERO_LIT =>
                Literal(tag, 0)
            case BOOL_LIT =>
                Literal(tag, in.nextByte)
            case STRING_LIT =>
                Literal(tag, in.nextNat)
            case BYTE_LIT |
                 CHAR_LIT |
                 SHORT_LIT |
                 INT_LIT |
                 LONG_LIT |
                 FLOAT_LIT |
                 DOUBLE_LIT =>
            	Literal(tag, in.nextLong)
           	case _ =>
           		error("unknown meta data tag: " + tag);
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
    case class Number(value: Long) extends AttribEntry;
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
    case class ConstantTypeRef(baseref: Int, numref: Int) extends AttribEntry;
    case class FlaggedType(flags: Int, tpe: Int) extends AttribEntry;
    case class Literal(tag: Int, data: Long) extends AttribEntry;

    case class SymbolInfo(name: Int, owner: Int, flags: Int, info: Int);
}
