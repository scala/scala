/*     ___ ____ ___   __   ___   ___
**    / _// __// _ | / /  / _ | / _ \    Scala classfile decoder
**  __\ \/ /__/ __ |/ /__/ __ |/ ___/    (c) 2003, LAMP/EPFL
** /____/\___/_/ |_/____/_/ |_/_/
**
**  $Id$
*/

package scala.tools.scalap;

import java.io.Writer;
import scala.collection.mutable.Buffer;


class ScalaWriter(args: Arguments, writer: Writer) extends CodeWriter(writer) {

    def printSymbol(sym: Symbol): Unit = sym match {
        case NoSymbol =>
            print("<nosymbol>")
        case s: TypeSymbol =>
            printFlags(s);
            print("type ");
            printTVar(s);
        case s: AliasSymbol =>
            printFlags(s);
            print("type " + s.name + " = ");
            printType(s.tpe);
        case s: ClassSymbol =>
            printFlags(s);
            if (Flags.is(Flags.DEFERRED, s.flags))
                print("/*deferred*/ ");
            if (Flags.is(Flags.OBJECT, s.flags))
                print("object " + s.name);
            else if (Flags.is(Flags.TRAIT, s.flags))
                print("trait " + s.name);
            else
                print("class " + s.name);
            printConstr(s.constr);
            print(" extends ");
            printType(s.tpe);
        case s: ValSymbol =>
            s.tpe match {
                case PolyType(tpe, Nil) =>
                    printFlags(s);
                    print("def " + s.name + ": ");
                    printType(tpe);
                case PolyType(_, _) =>
                    printFlags(s);
                    print("def " + s.name);
                    printType(s.tpe);
                case MethodType(_, _) =>
                    printFlags(s);
                    print("def " + s.name);
                    printType(s.tpe);
                case _ =>
                    printFlags(s);
                    print("val " + s.name + ": ");
                    printType(s.tpe);
            }
        case s: ExternalSymbol =>
            print("<externalsymbol: " + s.fullname + ">")
    }

    def printConstr(sym: Symbol): Unit = sym match {
        case s: ValSymbol =>
            s.tpe match {
                case PolyType(MethodType(argtpes, _), tvars) =>
                    print("[");
                    if (!tvars.isEmpty) {
                        printTVar(tvars.head);
                        tvars.tail foreach { sym =>
                            print(", ");
                            printTVar(sym);
                        }
                    }
                    print("]");
                    printTypes(argtpes, "(", ", ", ")");
                case MethodType(argtpes, _) =>
                    printTypes(argtpes, "(", ", ", ")");
                case _ =>
            }
    }

    def printScope(scope: Buffer[Symbol]): Unit = {
        var first = true;
        scope.elements foreach (
            sym => { sym match {
                case s: ValSymbol if
                    (s.tpe.isInstanceOf[OverloadedType] ||
                    (Flags.is(Flags.CASEACCESSOR, s.flags) &&
                    !s.tpe.isInstanceOf[PolyType])) =>
                case _ =>
                    if (!ignoreDef(sym)) {
                        if (first) print(" {").indent else print(";");
                        first = false;
                        newline;
                        printSymbol(sym);
                    }
            }});
        if (!first)
            newline.undent.print("}")
    }

    def printType(tpe: Type): Unit = {
        printType0(tpe);
        tpe match {
            case ThisType(_) => print(".type")
            case SingletonType(_, _) => print(".type")
            case _ =>
        }
    }

    def printTypes(tpes: List[Type], begin: String, infix: String, end: String): Unit = {
        if (!tpes.isEmpty)
            printTypes0(tpes, begin, infix, end);
    }

    def printType0(tpe: Type): Unit = tpe match {
        case NoType =>
        case ThisType(sym) => sym match {
            case x: ExternalSymbol => print(sym.fullname)
            case NoSymbol => print("this")
            case _ => print(sym.fullname).print(".this")
        }
        case SingletonType(tpe, sym) =>
            printPrefix(tpe);
            print(sym.name)
        case TypeRef(tpe, sym, args) =>
            printPrefix(tpe);
            print(sym.name);
            printTypes(args, "[", ", ", "]");
        case CompoundType(clazz, components) =>
            printTypes(components, "", " with ", "");
            if (clazz != NoSymbol)
                printScope(clazz.members);
        case MethodType(_, _) =>
            var tpe0 = tpe;
            while (tpe0.isInstanceOf[MethodType]) {
                tpe0 match {
                    case MethodType(argtpes, restpe) =>
                        printTypes0(argtpes, "(", ", ", ")");
                        tpe0 = restpe;
                }
            }
            print(":").newspace;
            printType(tpe0);
        case PolyType(tpe, tvars) =>
            print("[");
            if (!tvars.isEmpty) {
                printTVar(tvars.head);
                tvars.tail foreach (sym => {print(", "); printTVar(sym);});
            }
            print("]");
            printType(tpe);
        case OverloadedType(_, tpes) =>
            printTypes(tpes, "", " <and> ", "");
        case ConstantType(base, num) =>
            printType(base);
            print("(" + num + ")");
        case TypeFlag(TypeRef(_, _, List(tpe0)), flags) =>
            if (Flags.is(Flags.TF_DEF, flags))
                print("def ");
            printType(tpe0);
            if (Flags.is(Flags.TF_STAR, flags))
                print("*");
        case TypeFlag(tpe0, flags) =>
            if (Flags.is(Flags.TF_DEF, flags))
                print("def ");
            printType(tpe0);
        case _ => print("<unknown type>");
    }

    def printTypes0(tpes: List[Type], begin: String, infix: String, end: String): Unit = {
        print(begin);
        if (!tpes.isEmpty) {
            printType(tpes.head);
            tpes.tail foreach (t => { print(infix); printType(t) });
        }
        print(end);
    }

    def printPrefix(tpe: Type): Unit = tpe match {
        case NoType =>
        case ThisType(NoSymbol) =>
        case ThisType(sym) =>
            if ((sym.name.length() != 0) &&
                ("<root>" != sym.name)) {
                printType0(tpe);
                print(".")
            }
        case SingletonType(_, sym) =>
            if ((sym.name.length() != 0) &&
                ("<root>" != sym.name)) {
                printType0(tpe);
                print(".")
            }
        case TypeRef(_, _, _) =>
            printType0(tpe);
            print("#")
        case _ =>
            Console.println(tpe.getClass());
            printType0(tpe);
            print(".")
    }

    def printTVar(tvar: Symbol): Unit = tvar match {
        case sym: TypeSymbol =>
            if (Flags.is(Flags.COVAR, sym.flags))
            	print("+" + sym.name)
            else if (Flags.is(Flags.CONTRAVAR, sym.flags))
            	print("-" + sym.name);
            else
            	print(sym.name);
            if (!isExternalType(sym.tpe, "Any")) {
                print(" <: ")*;
                printType(sym.tpe);
            }
            if (!isExternalType(sym.lower, "All")) {
                print(" >: ")*;
                printType(sym.lower);
            }
    }

    def printFlags(sym: Symbol) = print(Flags.toString(sym.flags));

    def isExternalType(tpe: Type, name: String): Boolean = tpe match {
        case TypeRef(SingletonType(ThisType(root), pck), sym, Nil) =>
            root.name.equals("<root>") &&
            pck.name.equals("scala") &&
            sym.name.equals(name)
        case _ => false
    }

    def ignoreDef(s: Symbol) =
        (Flags.is(Flags.PRIVATE, s.flags) &&
         !((args != null) && (args contains "-private"))) ||
        (s.name == "<init>") ||
        Flags.is(Flags.CASEACCESSOR, s.flags);
}
