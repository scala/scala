/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $Id$

package scalac.atree;

import ch.epfl.lamp.util.CodePrinter;

import scalac.Unit;
import scalac.Global;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.SymbolTablePrinter;
import scalac.util.Debug;

/** This class provides methods to print attributed trees. */
public class ATreePrinter {

    //########################################################################
    // Private Fields

    /** The global environment */
    private final Global global;

    /** The underlying code printer */
    private final CodePrinter printer;

    /** The underlying symbol table printer */
    private final SymbolTablePrinter symtab;

    //########################################################################
    // Public Constructors

    /** Initalizes this instance */
    public ATreePrinter() {
        this(new CodePrinter());
    }

    /** Initalizes this instance */
    public ATreePrinter(String step) {
        this(Global.instance, new CodePrinter(step));
    }

    /** Initalizes this instance */
    public ATreePrinter(CodePrinter printer) {
        this(Global.instance, printer);
    }

    /** Initalizes this instance */
    public ATreePrinter(Global global, CodePrinter printer) {
        this.global = global;
        this.printer = printer;
        this.symtab = new SymbolTablePrinter(global, printer);
    }

    //########################################################################
    // Public Methods - Getting & Setting

    /** Returns the underlying code printer. */
    public CodePrinter getCodePrinter() {
        return printer;
    }

    //########################################################################
    // Public Methods - Formatting

    /** Increases the indentation level by one. */
    public ATreePrinter indent() {
        printer.indent();
        return this;
    }

    /** Decreases the indentation level by one. */
    public ATreePrinter undent() {
        printer.undent();
        return this;
    }

    /** Inserts a new line. */
    public ATreePrinter line() {
        printer.line();
        return this;
    }

    /** Inserts a white space. */
    public ATreePrinter space() {
        printer.space();
        return this;
    }

    /** Prints an opening brace followed by a new line. */
    public ATreePrinter lbrace() {
        return space().println('{').indent();
    }

    /** Prints a closing brace followed by a new line. */
    public ATreePrinter rbrace() {
        return undent().space().println('}');
    }

    //########################################################################
    // Public Methods - Printing simple values

    /** Prints a new line. */
    public ATreePrinter println() {
        printer.println();
        return this;
    }

    /** Prints the boolean value followed by a new line. */
    public ATreePrinter println(boolean value) {
        printer.println(value);
        return this;
    }

    /** Prints the byte value followed by a new line. */
    public ATreePrinter println(byte value) {
        printer.println(value);
        return this;
    }

    /** Prints the short value followed by a new line. */
    public ATreePrinter println(short value) {
        printer.println(value);
        return this;
    }

    /** Prints the char value followed by a new line. */
    public ATreePrinter println(char value) {
        printer.println(value);
        return this;
    }

    /** Prints the int value followed by a new line. */
    public ATreePrinter println(int value) {
        printer.println(value);
        return this;
    }

    /** Prints the long value followed by a new line. */
    public ATreePrinter println(long value) {
        printer.println(value);
        return this;
    }

    /** Prints the float value followed by a new line. */
    public ATreePrinter println(float value) {
        printer.println(value);
        return this;
    }

    /** Prints the double value followed by a new line. */
    public ATreePrinter println(double value) {
        printer.println(value);
        return this;
    }

    /** Prints the string followed by a new line. */
    public ATreePrinter println(String value) {
        printer.println(value);
        return this;
    }

    /** Prints the boolean value. */
    public ATreePrinter print(boolean value) {
        printer.print(value);
        return this;
    }

    /** Prints the byte value. */
    public ATreePrinter print(byte value) {
        printer.print(value);
        return this;
    }

    /** Prints the short value. */
    public ATreePrinter print(short value) {
        printer.print(value);
        return this;
    }

    /** Prints the char value. */
    public ATreePrinter print(char value) {
        printer.print(value);
        return this;
    }

    /** Prints the int value. */
    public ATreePrinter print(int value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public ATreePrinter print(long value) {
        printer.print(value);
        return this;
    }

    /** Prints the float value. */
    public ATreePrinter print(float value) {
        printer.print(value);
        return this;
    }

    /** Prints the long value. */
    public ATreePrinter print(double value) {
        printer.print(value);
        return this;
    }

    /** Prints the string. */
    public ATreePrinter print(String value) {
        printer.print(value);
        return this;
    }

    //########################################################################
    // Public Methods - Printing types and symbols

    /** Prints the symbol. */
    public ATreePrinter printSymbol(Symbol symbol) {
        symtab.printSymbolName(symbol);
        return this;
    }

    /** Prints the type. */
    public ATreePrinter printType(Type type) {
        symtab.printType(type);
        return this;
    }

    //########################################################################
    // Public Methods - Printing trees

    /** Prints the unit. */
    public ATreePrinter printUnit(Unit unit) {
        println("// Scala source: " + unit.source);
        return printRepository(unit.repository);
    }

    /** Prints the repository. */
    public ATreePrinter printRepository(ARepository repository) {
        AClass[] classes = repository.classes();
        for (int i = 0; i < classes.length; i++) printClass(classes[i]);
        return this;
    }

    /** Prints the class. */
    public ATreePrinter printClass(AClass clasz) {
        printClassModifiers(clasz);
        print(clasz.isInterface() ? "interface" : "class").space();
        printSymbol(clasz.symbol());
        Symbol[] tparams = clasz.tparams();
        if (tparams.length != 0) symtab.printTypeParams(tparams);
        Symbol[] vparams = clasz.vparams();
        if (vparams.length != 0) symtab.printValueParams(vparams);
        if (clasz.symbol().typeOfThis() != clasz.symbol().thisType())
            space().print(':').printType(clasz.symbol().typeOfThis());
        space().print("extends").space();
        symtab.printTypes(clasz.parents()," with ");
        lbrace();
        printRepository(clasz);
        AField[] fields = clasz.fields();
        for (int i = 0; i < fields.length; i++) printField(fields[i]);
        AMethod[] methods = clasz.methods();
        for (int i = 0; i < methods.length; i++) printMethod(methods[i]);
        return rbrace();
    }

    /** Prints the class modifiers. */
    public ATreePrinter printClassModifiers(AClass clasz) {
        if (clasz.isDeprecated()) print("deprecated").space();
        if (clasz.isSynthetic()) print("synthetic").space();
        if (clasz.isPublic()) print("public").space();
        if (clasz.isPrivate()) print("private").space();
        if (clasz.isProtected()) print("protected").space();
        if (clasz.isFinal()) print("final").space();
        if (clasz.isAbstract()) print("abstract").space();
        return this;
    }

    /** Prints the member modifiers. */
    public ATreePrinter printMemberModifiers(AMember member) {
        if (member.isDeprecated()) print("deprecated").space();
        if (member.isSynthetic()) print("synthetic").space();
        if (member.isPublic()) print("public").space();
        if (member.isPrivate()) print("private").space();
        if (member.isProtected()) print("protected").space();
        if (member.isStatic()) print("static").space();
        return this;
    }

    /** Prints the member code. */
    public ATreePrinter printMemberCode(AMember member) {
        if (member.code() == ACode.Void) return this;
        return print('=').space().printCode(member.code());
    }

    /** Prints the field. */
    public ATreePrinter printField(AField field) {
        printFieldModifiers(field);
        symtab.printSignature(field.symbol()).space();
        return printMemberCode(field).line();
    }

    /** Prints the field modifiers. */
    public ATreePrinter printFieldModifiers(AField field) {
        printMemberModifiers(field);
        if (field.isFinal()) print("final").space();
        if (field.isVolatile()) print("volatile").space();
        if (field.isTransient()) print("transient").space();
        return this;
    }

    /** Prints the method. */
    public ATreePrinter printMethod(AMethod method) {
        printMethodModifiers(method);
        symtab.printSignature(method.symbol()).space();
        return printMemberCode(method).line();
    }

    /** Prints the method modifiers. */
    public ATreePrinter printMethodModifiers(AMethod method) {
        printMemberModifiers(method);
        if (method.isFinal()) print("final").space();
        if (method.isSynchronized()) print("synchronized").space();
        if (method.isNative()) print("native").space();
        if (method.isAbstract()) print("abstract").space();
        return this;
    }

    /** Prints the code. */
    public ATreePrinter printCode(ACode code) {
        switch (code) {
        case Void:
            return print("<void>");
        case This(Symbol clasz):
            return printSymbol(clasz).print('.').print("this");
        case Constant(AConstant constant):
            return printConstant(constant);
        case Load(ALocation location):
            return printLocation(location);
        case Store(ALocation location, ACode value):
            printLocation(location).space().print('=').space();
            return printCode(value);
        case Apply(AFunction function, Type[] targs, ACode[] vargs):
            printFunction(function);
            if (targs.length > 0){
                print('[');
                for (int i = 0; i < targs.length; i++)
                    (i == 0 ? this : print(',').space()).printType(targs[i]);
                print(']');
            }
            print('(');
            for (int i = 0; i < vargs.length; i++)
                (i == 0 ? this : print(',').space()).printCode(vargs[i]);
            print(')');
            return this;
        case IsAs(ACode value, Type type, boolean cast):
            printCode(value).print('.');
            print(cast ? "asInstanceOf" : "isInstanceOf");
            return print('[').printType(type).print(']');
        case If(ACode test, ACode success, ACode failure):
            print("if").space().print('(').printCode(test).print(')').lbrace();
            printCode(success).line();
            rbrace().space().print("else").space().lbrace();
            printCode(failure).line();
            return rbrace();
        case Switch(ACode test, int[][] tags, ACode[] bodies, ACode other):
            print("switch").space().print('(').printCode(test).print(')');
            lbrace();
            for (int i = 0; i < tags.length; i++) {
                for (int j = 0; j < tags[i].length; j++)
                    print("case").space().print(tags[i][j]).print(':').line();
                indent().printCode(bodies[i]).undent().line();
            }
            print("case").space().print('_').print(':').line();
            indent().printCode(other).undent();
            return rbrace();
        case Synchronized(ACode lock, ACode value):
            print("synchronized").space();
            print('(').printCode(lock).print(')');
            return lbrace().printCode(value).rbrace();
        case Block(Symbol[] locals, ACode[] statements, ACode value):
            lbrace();
            for (int i = 0; i < locals.length; i++) {
                print("var").space().printSymbol(locals[i]);
                print(":").space().printType(locals[i].type());
                println(";");
            }
            for (int i = 0; i < statements.length; i++)
                printCode(statements[i]).println(';');
            return printCode(value).line().rbrace();
        case Label(Symbol label, Symbol[] locals, ACode value):
            print("label").space().printSymbol(label).print('(');
            for (int i = 0; i < locals.length; i++)
                (i == 0 ? this : print(',').space()).printSymbol(locals[i]);
            print(')').space().print('=').lbrace();
            return printCode(value).rbrace();
        case Goto(Symbol label, ACode[] vargs):
            print("goto").space().printSymbol(label).print('(');
            for (int i = 0; i < vargs.length; i++)
                (i == 0 ? this : print(',').space()).printCode(vargs[i]);
            return print(')');
        case Return(Symbol function, ACode value):
            print("return").symtab.printSymbolUniqueId(function).space();
            return printCode(value);
        case Throw(ACode value):
            return print("throw").space().printCode(value);
        case Drop(ACode value, Type type):
            print("drop").print('[').printType(type).print(']').space();
            return printCode(value);
        default:
            throw Debug.abort("unknown case", code);
        }
    }

    /** Prints the location. */
    public ATreePrinter printLocation(ALocation location) {
        switch (location) {
        case Module(Symbol module):
            return printSymbol(module);
        case Field(Void, Symbol field, true):
            return printSymbol(field.owner()).print('.').printSymbol(field);
        case Field(ACode object, Symbol field, boolean isStatic):
            printCode(object).print('.');
            if (isStatic) print("<static>").space();
            return printSymbol(field);
        case Local(Symbol local, _):
            return printSymbol(local);
        case ArrayItem(ACode array, ACode index):
            return printCode(array).print('(').printCode(index).print(')');
        default:
            throw Debug.abort("unknown case", location);
        }
    }

    /** Prints the function. */
    public ATreePrinter printFunction(AFunction function) {
        switch (function) {
        case Method(This(Symbol clasz), Symbol method, true):
            printSymbol(clasz).print('.').print("super").print('.');
            return printSymbol(method);
        case Method(Void, Symbol method, true):
            return printSymbol(method.owner()).print('.').printSymbol(method);
        case Method(ACode object, Symbol method, boolean isStatic):
            printCode(object).print('.');
            if (isStatic) print("<static>").space();
            return printSymbol(method);
        case Primitive(APrimitive primitive):
            return printPrimitive(primitive);
        default:
            throw Debug.abort("unknown case", function);
        }
    }

    /** Prints the primitive. */
    public ATreePrinter printPrimitive(APrimitive primitive) {
        switch (primitive) {
        case Negation(ATypeKind type):
            return printPrimitiveOp("NEG", type);
        case Test(ATestOp op, ATypeKind type, boolean zero):
            return printPrimitiveOp(op.toString() + (zero ? "Z" : ""), type);
        case Comparison(AComparisonOp op, ATypeKind type):
            return printPrimitiveOp(op.toString(), type);
        case Arithmetic(AArithmeticOp op, ATypeKind type):
            return printPrimitiveOp(op.toString(), type);
        case Logical(ALogicalOp op, ATypeKind type):
            return printPrimitiveOp(op.toString(), type);
        case Shift(AShiftOp op, ATypeKind type):
            return printPrimitiveOp(op.toString(), type);
        case Conversion(ATypeKind src, ATypeKind dst):
            return printPrimitiveOp("CONV", src, dst);
        case ArrayLength(ATypeKind type):
            return printPrimitiveOp("LENGTH", type);
        case StringConcat(ATypeKind lf, ATypeKind rg):
            return printPrimitiveOp("CONCAT", lf, rg);
        default:
            throw Debug.abort("unknown case", primitive);
        }
    }

    /** Prints the primitive operation of given type. */
    public ATreePrinter printPrimitiveOp(String op, ATypeKind type) {
        return printPrimitiveOp(op, type, null);
    }

    /** Prints the primitive operation of given types. */
    public ATreePrinter printPrimitiveOp(String op, ATypeKind t1,ATypeKind t2){
        print('<').print(op).print('>');
        if (t1 != null && global.uniqid) print('#').print(t1.toString());
        if (t2 != null && global.uniqid) print(',').print(t2.toString());
        return this;
    }

    /** Prints the constant. */
    public ATreePrinter printConstant(AConstant constant) {
        switch (constant) {
        case UNIT:
            return print("()");
        case BOOLEAN(boolean value):
            return print(value);
        case BYTE(byte value):
            return print(value);
        case SHORT(short value):
            return print(value);
        case CHAR(char value):
            return print('\'').print(value).print('\'');
        case INT(int value):
            return print(value);
        case LONG(long value):
            return print(value);
        case FLOAT(float value):
            return print(value);
        case DOUBLE(double value):
            return print(value);
        case STRING(String value):
            return print('\"').print(value).print('\"');
        case NULL:
            return print("null");
        case ZERO:
            return print("<zero>");
        default:
            throw Debug.abort("unknown case", constant);
        }
    }

    //########################################################################
    // Public Methods - Converting

    /** Returns the string representation of this printer. */
    public String toString() {
        return printer.toString();
    }

    //########################################################################
}
