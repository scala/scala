/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Evaluator.java,v 1.40 2002/10/04 15:37:10 paltherr Exp $
// $Id$

package scala.tools.scalai;

import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

import ch.epfl.lamp.util.SourceFile;

import scala.runtime.RunTime;

import scalac.symtab.Symbol;
import scalac.util.Debug;

public class Evaluator {

    //########################################################################
    // Private Classes

    // !!! remove ?
    // !!! is it correct to extend EvaluatorException?
    private static class LabelException extends EvaluatorException {

        public final Symbol symbol;
        public final Object[] args;

        public LabelException(Symbol symbol, Object[] args) {
            this.symbol = symbol;
            this.args = args;
        }
    }

    public static class EvaluationStack {

        public final EvaluationStack stack;
        public final SourceFile source;
        public final Symbol symbol;
        public final Object self;
        public final Object[] args;
        public final Object[] vars;

        public int pos;

        public EvaluationStack(EvaluationStack stack, CodeContainer code,
            Object self, Object[] args)
        {
            this.stack = stack;
            this.source = code.source;
            this.symbol = code.symbol;
            this.self = self;
            this.args = args;
            this.vars = new Object[code.stacksize];
        }

    }

    //########################################################################
    // Private Fields

    private final EvaluatorException trace;
    private EvaluationStack stack;

    //########################################################################
    // Public Constructors

    public Evaluator() {
        this.trace = new EvaluatorException();
        this.stack = null;
    }

    //########################################################################
    // Public Methods

    public Object toString(Object object) {
        try {
            return String.valueOf(object);
        } catch (Throwable exception) {
            return throw_(exception);
        }
    }

    public Object evaluate(Variable module) {
        return load(null, module);
    }

    public Object evaluate(Variable module, Function method, Object[] args) {
        return invoke(load(null, module), method, args);
    }

    public Object evaluate(CodeContainer code) {
        return evaluate(code, null, new Object[0]);
    }

    public Object evaluate(CodePromise code, Object object, Object[] args) {
        return evaluate(code.force(), object, args);
    }

    public Object evaluate(CodeContainer code, Object object, Object[] args) {
        return trace(code, object, args);
    }

    //########################################################################
    // Private Methods - trace

    private Object trace(CodeContainer code, Object object, Object[] args) {
        try {
            try {
                stack = new EvaluationStack(stack, code, object, args);
                return evaluate(code.code);
            } catch (EvaluatorException exception) {
                throw exception;
            } catch (Throwable exception) {
                return throw_(exception, "trace");
            }
        } catch (EvaluatorException exception) {
            exception.addScalaCall(stack.symbol, stack.source, stack.pos);
            throw exception;
        } finally {
            stack = stack.stack;
        }
    }

    //########################################################################
    // Private Methods - evaluate

    private Object evaluate(Code code) {
        switch (code) {

        case Block(Code[] stats, Code value):
            for (int i = 0; i < stats.length; i++) evaluate(stats[i]);
            return evaluate(value);

        case Label(Symbol symbol, Variable[] variables, Code expression):
            while (true)
                try {
                    return evaluate(expression);
                } catch (LabelException exception) {
                    if (exception.symbol != symbol) throw exception;
                    for (int i = 0; i < variables.length; i++) {
                        // !!! null
                        store(null, variables[i], exception.args[i]);
                    }
                }

        case If(Code cond, Code thenp, Code elsep):
            Object value = evaluate(cond);
            assert value instanceof Boolean : value.getClass();
            return evaluate(((Boolean)value).booleanValue() ? thenp : elsep);

        case Literal(Object value):
            return value;

        case Load(Code target, Variable variable):
            return load(evaluate(target), variable);

        case Store(Code target, Variable variable, Code expression):
            store(evaluate(target), variable, evaluate(expression));
            return RunTime.box();

        case Invoke(Code target, Function function, Code[] arguments, int pos):
            Object object = evaluate(target);
            Object[] args = new Object[arguments.length];
            for (int i = 0; i < args.length; i++)
                args[i] = evaluate(arguments[i]);
            stack.pos = pos;
            return invoke(object, function, args);

        case Create(ScalaTemplate template):
            return invoke(null, template.getConstructor(),
                new Object[] {template.getHandler()});

        case IsScala(Code target, Symbol symbol):
            Object object = evaluate(target);
            if (object == null || !isScalaObject(object)) return Boolean.FALSE;
            Symbol actual = getScalaObject(object).template.getSymbol();
            return new Boolean(actual.isSubClass(symbol));

        case IsJava(Code target, Class clasz):
            Object object = evaluate(target);
            return new Boolean(clasz.isInstance(object));

        case Or(Code lf, Code rg):
            Object object = evaluate(lf);
            assert object instanceof scala.Boolean : object.getClass();
            boolean value = ((scala.Boolean)object).asBoolean();
            if (value) return new Boolean(value);
            return evaluate(rg);

        case And(Code lf, Code rg):
            Object object = evaluate(lf);
            assert object instanceof scala.Boolean : object.getClass();
            boolean value = ((scala.Boolean)object).asBoolean();
            if (!value) return new Boolean(value);
            return evaluate(rg);

        case Null:
            return null;

        case Self:
            return stack.self;

        default:
            throw Debug.abort("illegal code", code);
        }
    }

    //########################################################################
    // Private Methods - invoke

    private Object invoke(Object object, Function function, Object[] args) {
        switch (function) {

        case Global(CodePromise code):
            return evaluate(code, object, args);

        case Member(Symbol symbol):
            return getScalaObject(object).invoke(object, symbol, args);

        case Label(Symbol symbol):
            throw new LabelException(symbol, args);

        case JavaConstructor(Constructor constructor):
            return invoke(object, constructor, args);

        case JavaMethod(Method method):
            return invoke(object, method, args);

        case Pos:
            if (object instanceof scala.Int) {
                int value = ((scala.Int)object).asInt();
                return new Integer(value);
            } else if (object instanceof scala.Long) {
                long value = ((scala.Long)object).asLong();
                return new Long(value);
            } else if (object instanceof scala.Float) {
                float value = ((scala.Float)object).asFloat();
                return new Float(value);
            } else {
                double value = ((scala.Double)object).asDouble();
                return new Double(value);
            }

        case Neg:
            if (object instanceof scala.Int) {
                int value = ((scala.Int)object).asInt();
                return new Integer(-value);
            } else if (object instanceof scala.Long) {
                long value = ((scala.Long)object).asLong();
                return new Long(-value);
            } else if (object instanceof scala.Float) {
                float value = ((scala.Float)object).asFloat();
                return new Float(-value);
            } else {
                double value = ((scala.Double)object).asDouble();
                return new Double(-value);
            }

        case Throw:
            assert args.length == 0 : Debug.show(args);
            assert object instanceof Throwable : object.getClass();
            return throw_((Throwable)object);

        case StringPlus:
            assert args.length == 1 : Debug.show(args);
            //assert object instanceof String : object.getClass().getName();
            return (String.valueOf(object)).concat(String.valueOf(args[0]));

        case EqEq:
            assert args.length == 1 : Debug.show(args);
            return object == null ? new Boolean(args[0] == null) : new Boolean(object.equals(args[0])); // !!!

        case BangEq:
            assert args.length == 1 : Debug.show(args);
            return object == null ? new Boolean(args[0] != null) : new Boolean(!object.equals(args[0])); // !!!

        case HashCode:
            assert args.length == 0 : Debug.show(args);
            return new Integer(getHandlerObject(object).hashCode());

        case ToString:
            assert args.length == 0 : Debug.show(args);
            return getHandlerObject(object).toString();

        default:
            throw Debug.abort("illegal function", function);
        }
    }

    private Object invoke(Object object, Constructor constructor,Object[]args){
        try {
            return constructor.newInstance(args);
        } catch (StackOverflowError exception) {
            return throw_(exception);
        } catch (ExceptionInInitializerError exception) {
            return throw_(exception);
        } catch (InvocationTargetException exception) {
            return throw_(exception);
        } catch (InstantiationException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  constr = " + Debug.show(constructor);
            String msg3 = "\n  args   = " + Debug.show(args);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        } catch (IllegalAccessException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  constr = " + Debug.show(constructor);
            String msg3 = "\n  args   = " + Debug.show(args);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        } catch (IllegalArgumentException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  constr = " + Debug.show(constructor);
            String msg3 = "\n  args   = " + Debug.show(args);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        }
    }

    private Object invoke(Object object, Method method, Object[] args) {
        try {
            return method.invoke(object, args);
        } catch (StackOverflowError exception) {
            return throw_(exception);
        } catch (NullPointerException exception) {
            return throw_(exception);
        } catch (ExceptionInInitializerError exception) {
            return throw_(exception);
        } catch (InvocationTargetException exception) {
            return throw_(exception);
        } catch (IllegalAccessException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  method = " + Debug.show(method);
            String msg3 = "\n  args   = " + Debug.show(args);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        } catch (IllegalArgumentException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  method = " + Debug.show(method);
            String msg3 = "\n  args   = " + Debug.show(args);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        }
    }

    //########################################################################
    // Private Methods - store

    private Object store(Object object, Variable variable, Object value) {
        switch (variable) {

        case Global(_):
            return ((Variable.Global)variable).value = value;

        case Module(_, _):
            return ((Variable.Module)variable).value = value;

        case Member(int index):
            return getScalaObject(object).variables[index] = value;

        case Argument(int index):
            return stack.args[index] = value;

        case Local(int index):
            return stack.vars[index] = value;

        case JavaField(Field field):
            return store(object, field, value);

        default:
            throw Debug.abort("illegal variable", variable);
        }
    }

    private Object store(Object object, Field field, Object value) {
        try {
            field.set(object, value);
            return value;
        } catch (NullPointerException exception) {
            return throw_(exception);
        } catch (ExceptionInInitializerError exception) {
            return throw_(exception);
        } catch (IllegalAccessException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  field  = " + Debug.show(field);
            String msg3 = "\n  value  = " + Debug.show(value);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        } catch (IllegalArgumentException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  field  = " + Debug.show(field);
            String msg3 = "\n  value  = " + Debug.show(value);
            throw Debug.abort(msg1 + msg2 + msg3, exception);
        }
    }

    //########################################################################
    // Private Methods - load

    private Object load(Object object, Variable variable) {
        switch (variable) {

        case Global(Object value):
            return value;

        case Module(CodePromise body, Object value):
            if (value != null) return value;
            ((Variable.Module)variable).body = null;
            evaluate(body, null, new Object[0]);
            return load(object, variable);

        case Member(int index):
            return getScalaObject(object).variables[index];

        case Argument(int index):
            return stack.args[index];

        case Local(int index):
            return stack.vars[index];

        case JavaField(Field field):
            return load(object, field);

        default:
            throw Debug.abort("illegal variable", variable);
        }
    }

    private Object load(Object object, Field field) {
        try {
            return field.get(object);
        } catch (NullPointerException exception) {
            return throw_(exception);
        } catch (ExceptionInInitializerError exception) {
            return throw_(exception);
        } catch (IllegalAccessException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  field  = " + Debug.show(field);
            throw Debug.abort(msg1 + msg2, exception);
        } catch (IllegalArgumentException exception) {
            String msg1 = "\n  object = " + Debug.show(object);
            String msg2 = "\n  field  = " + Debug.show(field);
            throw Debug.abort(msg1 + msg2, exception);
        }
    }

    //########################################################################
    // Private Methods - throw

    private Object throw_(Throwable exception) {
        return throw_(exception, null);
    }

    private Object throw_(Throwable exception, String method) {
        if (exception.getCause() != null && (
                exception instanceof ExceptionInInitializerError ||
                exception instanceof InvocationTargetException))
            exception = exception.getCause();
        if (trace.getCause() != exception) trace.reset(exception);
        trace.addScalaLeavePoint(getClass().getName(), method);
        throw trace;
    }

    //########################################################################
    // Private Methods - !!!

    private boolean isScalaObject(Object object) {
        return Proxy.isProxyClass(object.getClass()); // !!! add more checks ?
    }

    private ScalaObject getScalaObject(Object object) {
        Object handler = getHandlerObject(object);
        assert handler instanceof ScalaObject : handler.getClass();
        return (ScalaObject)handler;
    }

    private Object getHandlerObject(Object object) {
        if (object == null) return throw_(new NullPointerException());
        assert object instanceof Proxy : object.getClass();
        return Proxy.getInvocationHandler(object);
    }

    //########################################################################
}
