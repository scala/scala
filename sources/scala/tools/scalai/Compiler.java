/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Compiler.java,v 1.19 2002/10/01 16:14:07 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Proxy;
import java.lang.reflect.InvocationHandler;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;
import java.util.Iterator;

import scalac.Unit;
import scalac.Global;
import scalac.ast.Tree;
import scalac.ast.Tree.ValDef;
import scalac.ast.TreeGen;
import scalac.symtab.Definitions;
import scalac.symtab.Type;
import scalac.symtab.Symbol;
import scalac.symtab.TermSymbol;
import scalac.util.Debug;
import scalac.util.Name;
import scalac.util.Position;

public class Compiler {

    //########################################################################

    private final Symbol WRITER;
    private final Symbol WRITER_WRITE;
    private final Type   SYMBOL_TYPE;

    //########################################################################

    private final Global global;
    private final Definitions definitions;
    private final TreeGen make;
    private final Constants constants;
    private final ClassLoader loader;
    private final Environment environment;
    private final Evaluator evaluator; // !!! remove
    private final SymbolWriter writer;
    private final Map any_methods;

    private boolean interactive;

    public Compiler(Global global, Evaluator evaluator, SymbolWriter writer) {
        this.global = global;
        this.definitions = global.definitions;
        this.make = global.treeGen;
        this.constants = new Constants();
        this.loader = new PathClassLoader(global.classPath);
        scala.runtime.RunTime.setClassLoader(loader);
        JavaMirror mirror = new JavaMirror(definitions, loader);
        this.environment = new Environment(this, mirror);
        this.evaluator = evaluator;
        this.writer = writer;
        this.any_methods = new HashMap();

        Name WRITER_N = Name.fromString(SymbolWriter.class.getName());
        Type WRITER_TYPE = definitions.getType(WRITER_N);
        this.WRITER = newGlobalVariable(WRITER_TYPE, writer);
        this.WRITER_WRITE = WRITER_TYPE.lookup(Name.fromString("write2"));

        Name SYMBOL_N = Name.fromString(Symbol.class.getName());
        this.SYMBOL_TYPE = definitions.getType(SYMBOL_N);

        environment.insertFunction(definitions.STRING_PLUS_ANY, Function.StringPlus); // !!!
        // !!! ANY_PLUS_STRING is commented out in definitions
        // !!! environment.insertFunction(definitions.ANY_PLUS_STRING, Function.StringPlus); // !!!
        environment.insertFunction(definitions.THROW, Function.Throw);
        // !!! environment.insertFunction(definitions.MATCH, ...);
        // !!! environment.insertFunction(definitions.IS, ...);
        // !!! environment.insertFunction(definitions.AS, ...);

        java.lang.reflect.Method getInvocationHandler_method = null;
        try {
            getInvocationHandler_method = Proxy.class.getMethod(
                "getInvocationHandler",
                new Class[] {Object.class});
        } catch (Exception exception) {
            throw Debug.abort("getInvocationHandler", exception);
        }

        // !!! method java.lang.Object.clone()

        java.lang.reflect.Method equals_method = null;
        try {
            equals_method = Object.class.getMethod("equals",
                new Class[] {Object.class});
        } catch (Exception exception) {
            throw Debug.abort("equals", exception);
        }
        CodePromise equals_code = new CodePromise(
            new CodeContainer(
                null, // !!!
                Code.Invoke(
                    Code.Invoke(
                        Code.Null,
                        Function.JavaMethod(getInvocationHandler_method),
                        new Code[] {
                            Code.Self},
                        Position.NOPOS, null // !!!
                    ),
                    Function.JavaMethod(equals_method),
                    new Code[] {
                        Code.Load(
                            Code.Null, Variable.Context(Evaluator.Levels.ARGS, 0))},
                    Position.NOPOS, null), // !!!
                0));
        // !!! any_methods.put(_, equals_code);
        any_methods.put(equals_method, equals_code);

        // !!! method java.lang.Object.equals(Object)
        // !!! method java.lang.Object.finalize()
        // !!! method java.lang.Object.getClass()

        java.lang.reflect.Method hashCode_method = null;
        try {
            hashCode_method = Object.class.getMethod("hashCode", new Class[0]);
        } catch (Exception exception) {
            throw Debug.abort("hashCode", exception);
        }
        CodePromise hashCode_code = new CodePromise(
            new CodeContainer(
                definitions.HASHCODE,
                Code.Invoke(
                    Code.Self, Function.HashCode, new Code[0],
                    Position.NOPOS, definitions.HASHCODE),
                0));
        any_methods.put(definitions.HASHCODE, hashCode_code);
        any_methods.put(hashCode_method, hashCode_code);
        environment.insertFunction(definitions.HASHCODE,
            Function.JavaMethod(hashCode_method));
        Override hashCode_override = Override.empty();
        hashCode_override.insert(hashCode_method).insert(definitions.HASHCODE);
        environment.insertOverride(definitions.HASHCODE, hashCode_override);

        // !!! method java.lang.Object.notify()
        // !!! method java.lang.Object.notifyAll()

        java.lang.reflect.Method toString_method = null;
        try {
            toString_method = Object.class.getMethod("toString", new Class[0]);
        } catch (Exception exception) {
            throw Debug.abort("toString", exception);
        }
        CodePromise toString_code = new CodePromise(
            new CodeContainer(
                definitions.TOSTRING,
                Code.Invoke(
                    Code.Self, Function.ToString, new Code[0],
                    Position.NOPOS, definitions.TOSTRING),
                0));
        any_methods.put(definitions.TOSTRING, toString_code);
        any_methods.put(toString_method, toString_code);
        environment.insertFunction(definitions.TOSTRING,
            Function.JavaMethod(toString_method));
        Override toString_override = Override.empty();
        toString_override.insert(toString_method).insert(definitions.TOSTRING);
        environment.insertOverride(definitions.TOSTRING, toString_override);

        // !!! method java.lang.Object.wait()
        // !!! method java.lang.Object.wait(long)
        // !!! method java.lang.Object.wait(long, int)

        java.lang.reflect.Method bang_method = null;
        try {
            bang_method = scala.Boolean.class.getMethod("$bang",
                new Class[0]);
        } catch (Exception exception) {
            throw Debug.abort("$bang", exception);
        }

        CodePromise eqeq_code = new CodePromise(
            new CodeContainer(
                definitions.EQEQ,
                Code.Invoke(
                    Code.Self, Function.JavaMethod(equals_method), new Code[] {
                        Code.Load(
                            Code.Null,
                            Variable.Context(Evaluator.Levels.ARGS, 0)) },
                    Position.NOPOS, definitions.EQEQ),
                0));
        any_methods.put(definitions.EQEQ, eqeq_code);
        environment.insertFunction(definitions.EQEQ, Function.EqEq);
        Override eqeq_override = Override.empty().insert(definitions.EQEQ);
        environment.insertOverride(definitions.EQEQ, eqeq_override);

        CodePromise bangeq_code = new CodePromise(
            new CodeContainer(
                definitions.BANGEQ,
                Code.Invoke(
                    Code.Box(
                        Code.Invoke(
                            Code.Self, Function.EqEq, new Code[] {
                                Code.Load(
                                    Code.Null,
                                    Variable.Context(Evaluator.Levels.ARGS, 0)) },
                            Position.NOPOS, definitions.BANGEQ)),
                    Function.JavaMethod(bang_method),
                    new Code[0],
                    Position.NOPOS, definitions.BANGEQ),
                0));
        any_methods.put(definitions.BANGEQ, bangeq_code);
        environment.insertFunction(definitions.BANGEQ, Function.BangEq);
        Override bangeq_override = Override.empty().insert(definitions.BANGEQ);
        environment.insertOverride(definitions.BANGEQ, bangeq_override);
    }

    //########################################################################

    public ScalaTemplate load(Symbol symbol, Tree.ClassDef tree) {
        assert tree.tparams   .length == 0 : Debug.show(tree);
        assert tree.vparams   .length == 1 : Debug.show(tree);
        assert tree.vparams[0].length == 0 : Debug.show(tree);
        return compileTemplate(symbol, tree.impl.body);
    }

    public ScalaTemplate compileTemplate(Symbol symbol, Tree[] body) {
        Set supertypes = new HashSet();
        List interfaces = new ArrayList();
        getTypes(supertypes, interfaces, symbol);
        Class[] proxytype = new Class[interfaces.size()];
        interfaces.toArray(proxytype);
        Class proxy = Proxy.getProxyClass(loader, proxytype);
        Function constructor = getConstructor(proxy);
        Type[] bases = symbol.parents();
        Map methods;
        List fields;
        if (symbol.isInterface() || bases.length == 0) {
            methods = new HashMap(any_methods);
            fields = new ArrayList();
        } else {
            Template template = environment.lookupTemplate(bases[0].symbol());
            switch (template) {

            case Global(ScalaTemplate template_): // !!!
                methods = template_.getMethods();
                fields = template_.getFields();
                break;

            case JavaClass(_):
                methods = new HashMap(any_methods);
                fields = new ArrayList();
                break;

            default:
                throw Debug.abort("illegal case", template);
            }
        }
        for (int i = 0; i < body.length; i++) {
            addTemplateMember(methods, fields, body[i]);
        }
        return new ScalaTemplate(evaluator, symbol, constructor, methods, fields.toArray());
    }

    //########################################################################

    public CodePromise compile(Symbol symbol, Tree.DefDef tree) {
        assert tree.tparams.length == 0 : Debug.show(tree);
        assert tree.vparams.length == 1 : Debug.show(tree);
        return new CodePromise(new ScalaFunction(this, symbol, tree.vparams[0],tree.rhs));
    }

    //########################################################################
    //

    public CodeContainer compile(Symbol owner, Tree tree) {
        return compile(owner, tree, new Symbol[0]); // !!!
    }

    public CodeContainer compile(Symbol owner, Tree tree, ValDef[] params) {
        return compile(owner, tree, Tree.symbolOf(params));
    }

    public CodeContainer compile(Symbol owner, Tree tree, Symbol[] params) {
        ExpressionContext context = new ExpressionContext(environment, owner);
        ExpressionCompiler worker = new ExpressionCompiler(definitions, constants, context, params);
        return worker.compile(tree);
    }

    public CodeContainer compile(Symbol owner, ArrayList items) {
        return compile(owner, items, new Symbol[0]); // !!!
    }

    public CodeContainer compile(Symbol owner, ArrayList items,Symbol[]params){
        ExpressionContext context = new ExpressionContext(environment, owner);
        ExpressionCompiler worker = new ExpressionCompiler(definitions, constants, context, params);
        return worker.compile(items);
    }

    //########################################################################
    // Private Methods -

    public CodeContainer compile(Unit[] units, boolean interactive) {
        this.interactive = interactive;
        ArrayList buffer = new ArrayList();
        for (int i = 0; i < units.length; i++) declare(buffer, units[i].body);
        return compile(null, buffer);
    }

    //########################################################################
    // Private Methods -

    private void declare(ArrayList buffer, Tree[] trees) {
        for (int i = 0; i < trees.length; i++) declare(buffer, trees[i]);
    }

    private void declare(ArrayList buffer, Tree tree) {
        Symbol symbol = tree.symbol();
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, _):
            environment.insertClassDef(symbol, (Tree.ClassDef)tree);
            return;

        // !!! these could be removed
        case PackageDef(Tree packaged, Tree.Template(Tree[] bases, Tree[] body)):
            assert packaged.symbol().isPackage() : Debug.show(tree); // !!! was isJavaPackage
            assert bases.length == 0 : Debug.show(tree);
            declare(buffer, body);
            return;

        case ValDef(_, _, _, Tree body):
            if (symbol.isModule()) {
                environment.insertVariable(symbol, Variable.Module(new CodePromise(new ModuleBuilder(this, symbol, body)), null));
                if (mustShowDefinition(symbol)) writer.write1(symbol);
            } else {
                Variable variable = environment.insertVariable(symbol, Variable.Global(constants.zero(symbol.type())));
                if (body != Tree.Empty) buffer.add(make.Assign(make.Ident(symbol), body));
                if (mustShowDefinition(symbol)) buffer.add(
                    make.Apply(make.Select(make.Ident(WRITER), WRITER_WRITE),
                        new Tree[] {
                            make.Ident(newGlobalVariable(SYMBOL_TYPE, symbol)),
                            make.Ident(symbol)}));
            }
            return;

        case DefDef(_, _, _, _, _, _):
            CodePromise function = compile(symbol, (Tree.DefDef)tree);
            environment.insertFunction(symbol, Function.Global(function));
            if (isEntryPoint(symbol)) {
                buffer.add(make.Apply(make.Ident(symbol), new Tree[0]));
            } else if (mustShowDefinition(symbol)) {
                writer.write1(symbol);
            }
            return;

        default:
            buffer.add(tree);
            return;
        }
    }

    //########################################################################
    // Private Methods -

    // !!! move elsewhere ?
    public static final Name MAIN_N = Name.fromString("main");

    private boolean isEntryPoint(Symbol symbol) {
        if (symbol.name != MAIN_N) return false;
        switch (symbol.type()) {
        case MethodType(Symbol[] vparams, _): return vparams.length == 0;
        default: return true;
        }
    }

    private boolean mustShowDefinition(Symbol symbol) {
        // !!! the last test is to avoid printing of lifted symbols
        return
            (interactive &&
                // !!! symbol.owner() == definitions.CONSOLE_CLASS &&
            writer != null && symbol.name.lastPos((byte)'$') < 0); // !!! '$'
    }

    private Symbol newGlobalVariable(Type type, Object value) {
        Symbol symbol = new TermSymbol(Position.NOPOS,
            Name.fromString(value.toString()), definitions.ROOT, 0);
        symbol.setInfo(type);
        environment.insertVariable(symbol, Variable.Global(value));
        return symbol;
    }

    //########################################################################
    // Private Methods - template types

    private void getTypes(Set supertypes, List interfaces, Symbol type) {
        if (supertypes.contains(type)) return; else supertypes.add(type);
        if (!type.isJava()) {
            Type[] basetypes = type.parents();
            for (int i = 0; i < basetypes.length; i++) {
                getTypes(supertypes, interfaces, basetypes[i].symbol());
            }
            return;
        } else {
        Template template = environment.lookupTemplate(type);
        switch (template) {
        case JavaClass(Class clasz):
            getTypes(supertypes, interfaces, clasz);
            return;
        default:
            Debug.abort("illegal template", template);
        }
        }
    }

    private void getTypes(Set supertypes, List interfaces, Class type) {
        if (supertypes.contains(type)) return; else supertypes.add(type);
        if (type.isInterface()) {
            interfaces.add(type);
        } else {
            Class supertype = type.getSuperclass();
            if (supertype != null) getTypes(supertypes, interfaces, supertype);
            Class[] basetypes = type.getInterfaces();
            for (int i = 0; i < basetypes.length; i++) {
                getTypes(supertypes, interfaces, basetypes[i]);
            }
        }
    }

    //########################################################################
    // Private Methods - template constructors

    private static final Class[] CSTR_PARAMS =
        new Class[] { InvocationHandler.class }; // !!! remove ?

    private Function getConstructor(Class proxy) {
        try {
            return Function.JavaConstructor(proxy.getConstructor(CSTR_PARAMS));
        } catch (NoSuchMethodException exception) {
            throw Debug.abort(exception);
        }
    }

    //########################################################################
    // Private Methods - template members

    private void addTemplateMember(Map methods, List fields, Tree tree) {
        Symbol symbol = tree.symbol();
        switch (tree) {

        case Empty:
            return;

        case ClassDef(_, _, _, _, _, _):
            environment.insertClassDef(symbol, (Tree.ClassDef)tree);
            return;

        case ValDef(_, _, _, Tree body):
            assert body == Tree.Empty : Debug.show(tree);
            assert !symbol.isModule() : Debug.show(tree);
            int index = fields.size();
            fields.add(constants.zero(symbol.type()));
            environment.insertVariable(symbol, Variable.Member(index));
            return;

        case DefDef(_, _, _, _, _, _):
            assert !methods.containsKey(symbol) : Debug.show(symbol);
            CodePromise function = compile(symbol, (Tree.DefDef)tree);
            Override override = environment.lookupOverride(symbol);
            for (Iterator i = override.iterator(); i.hasNext(); ) {
                methods.put(i.next(), function);
            }
            environment.insertFunction(symbol, Function.Member(symbol));
            return;

        default:
            throw Debug.abort("illegal tree", tree);
        }
    }

    //########################################################################
    // !!!

    public static class ModuleBuilder extends CodeGenerator {

        private final Compiler compiler;
        private final Symbol symbol;
        private final Tree body;

        public ModuleBuilder(Compiler compiler, Symbol symbol, Tree body) {
            this.compiler = compiler;
            this.symbol = symbol;
            this.body = body;
        }

        public CodeContainer generate() {
            return compiler.compile(symbol, body);
        }
    }

    //########################################################################
}
