/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import scala.collection.mutable._;


class EntityTable(attrib: ScalaAttribute) {
    import attrib._;

    val table: Array[Entity] = new Array(attrib.table.length);
    var root: Buffer[Symbol] = new Buffer;

    {
        //Console.println("created table");
        var i = 0;
        while (i < attrib.table.length) {
            table(i) = attrib.table(i) match {
                case TermName(str) => Text(Names.decode(str));
                case TypeName(str) => Text(Names.decode(str));
                case Number(x) => Value(x);
                case _ => null;
            }
            i = i + 1;
        }
        //Console.println("decoded names");
        i = 0;
        var fixupIds: List[Int] = Nil;
        while (i < attrib.table.length) {
            table(i) = attrib.table(i) match {
                case NoneSym() =>
                    NoSymbol
                case TypeSym(SymbolInfo(nameId, _, flags, _), _) =>
                    fixupIds = i :: fixupIds;
                    new TypeSymbol(getText(nameId), flags)
                case AliasSym(SymbolInfo(nameId, _, flags, _), _) =>
                    fixupIds = i :: fixupIds;
                    new AliasSymbol(getText(nameId), flags)
                case ClassSym(SymbolInfo(nameId, _, flags, _), _, _) =>
                    fixupIds = i :: fixupIds;
                    new ClassSymbol(getText(nameId), flags)
                case ValSym(SymbolInfo(nameId, _, flags, _), _) =>
                    fixupIds = i :: fixupIds;
                    new ValSymbol(getText(nameId), flags)
                case ExtRef(mod, nameId, _) =>
                    fixupIds = i :: fixupIds;
                    new ExternalSymbol(getText(nameId), mod)
                case _ =>
                    table(i)
            }
            i = i + 1;
        }
        //Console.println("created symbols");
        i = 0;
        while (i < attrib.table.length) {
            val x = getType(i);
            i = i + 1;
        }
        //Console.println("created types");
        def fix(i: Int, info: SymbolInfo): Symbol = {
            val sym = getSymbol(i);
            sym.fix(getType(info.info), getSymbol(info.owner));
            sym
        }
        fixupIds foreach {
            i => attrib.table(i) match {
                case TypeSym(info, loId) =>
                    fix(i, info).fix(getType(loId));
                case AliasSym(info, constrId) =>
                    fix(i, info).fix(getSymbol(constrId));
                case ClassSym(info, typeId, constrId) =>
                    val sym = fix(i, info);
                    sym.fix(getType(typeId));
                    sym.fix(getSymbol(constrId));
                    sym.owner match {
                        case x: ExternalSymbol => root += sym;
                        case _ =>
                    }
                case ValSym(info, classId) =>
                    fix(i, info).fix(getSymbol(classId));
                case ExtRef(_, _, ownerId) =>
                    getSymbol(i).fix(getSymbol(ownerId));
            }
        }
    }

    def getText(i: Int): String = table(i) match {
        case Text(str) => str;
    }

    def getValue(i: Int): Long = table(i) match {
        case Value(x) => x;
    }

    def getSymbol(i: Int): Symbol =
        if (i < 0) NoSymbol else table(i).asInstanceOf[Symbol];

    def getSymbols(is: List[Int]): List[Symbol] = is map {i => getSymbol(i)};

    def getType(i: Int): Type = {
        if (i < 0)
            NoType
        else if (table(i) != null) {
            if (table(i).isInstanceOf[Type])
                table(i).asInstanceOf[Type]
            else
                NoType
        } else {
            val res: Type = attrib.table(i) match {
                case NoneType() =>
                    NoType
                case SelfType(symId) =>
                    ThisType(getSymbol(symId))
                case SingleType(typeId, symId) =>
                    SingletonType(getType(typeId), getSymbol(symId))
                case TypeReference(typeId, symId, argIds) =>
                    TypeRef(getType(typeId), getSymbol(symId), getTypes(argIds))
                case CompoundTypeRef(symId, typeIds) =>
                    CompoundType(getSymbol(symId), getTypes(typeIds))
                case MethodTypeRef(restypeId, argtypeIds) =>
                    MethodType(getTypes(argtypeIds), getType(restypeId))
                case PolyTypeRef(typeId, symIds) =>
                    PolyType(getType(typeId), getSymbols(symIds))
                case OverloadedTypeRef(ids) =>
                    val Pair(symIds, typeIds) = ids partition {
                        i => (table(i) != null) && table(i).isSymbol
                    }
                    OverloadedType(getSymbols(symIds), getTypes(typeIds))
                case ConstantTypeRef(baseId, valId) =>
                    ConstantType(getType(baseId), getValue(valId))
                case FlaggedType(flags, typeId) =>
                    TypeFlag(getType(typeId), flags)
            }
            table(i) = res;
            res
        }
    }

    def getTypes(is: List[Int]): List[Type] = is map {i => getType(i)};

    def print: Unit = {
        Console.println("ROOT = " + root);
        var i = 0;
        while (i < table.length) {
            Console.println("" + i + ": " + table(i).toSource);
            Console.println("    " + table(i));
            i = i + 1;
        }
    }
}
