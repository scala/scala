/*     ____ ____  ____ ____  ______                                     *\
**    / __// __ \/ __// __ \/ ____/    SOcos COmpiles Scala             **
**  __\_ \/ /_/ / /__/ /_/ /\_ \       (c) 2002, LAMP/EPFL              **
** /_____/\____/\___/\____/____/                                        **
\*                                                                      */

// $OldId: Evaluator.java,v 1.40 2002/10/04 15:37:10 paltherr Exp $
// $Id$

package scalai;

import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;

import scala.runtime.RunTime;

import scalac.symtab.Symbol;
import scalac.util.Debug;

public class Evaluator {

    //########################################################################
    // Public Interfaces

    // !!! remove
    public static interface Levels {
        public static final int CLASS = 0;
        public static final int ARGS = 1; // !!! use PARAM ?
        public static final int BODY = 2;
    }

    //########################################################################
    // Private Classes

    // !!! remove ?
    private static class LabelException extends EvaluatorException {

        public final Symbol symbol;
        public final Object[] args;

        public LabelException(Symbol symbol, Object[] args) {
            super(null);
            this.symbol = symbol;
            this.args = args;
        }
    }

    //########################################################################
    // Private Fields

    private final Object[][] variables;
    private Object self;

    //########################################################################
    // Public Constructors

    public Evaluator() {
        this.variables = new Object[3][];
        this.variables[Levels.CLASS] = null;
        this.variables[Levels.ARGS] = null;
        this.variables[Levels.BODY] = null;
        this.self = null;
    }

    //########################################################################
    // Public Methods

    public Object evaluate(CodeContainer code) {
        // !!! System.out.println("!!! EVALUATE");
        // !!! System.out.println("!!! size           = " + size);
        // !!! System.out.println("!!! code.symbol    = " +Debug.show(code.symbol));
        // !!! System.out.println("!!! code.stacksize = " + code.stacksize);
        // !!! System.out.println("!!! code.code      = " + code.code);
        // !!! System.out.println("!!! ");
        return evaluate(code, null, new Object[0]);
    }

    public Object evaluate(CodePromise code, Object object, Object[] args) {
        return evaluate(code.force(), object, args);
    }

    public Object evaluate(CodeContainer code, Object object, Object[] args) {
        try {
        // !!! System.out.println("!!! EVALUATE");
        // !!! System.out.println("!!! code.symbol    = " +Debug.show(code.symbol));
        // !!! for (int i = 0; i < args.length; i++) {
        // !!!     System.out.println("!!! arg" + i + "           = " + Debug.show(args[i]));
        // !!! }
        // !!! System.out.println("!!! code.stacksize = " + code.stacksize);
        // !!! System.out.println("!!! code.code      = " + code.code);
        // !!! System.out.println("!!! ");

        Object backup_self = self;
        Object[] backup_args = variables[Levels.ARGS];
        Object[] backup_body = variables[Levels.BODY];
        self = object;
        variables[Levels.ARGS] = args;
        variables[Levels.BODY] = new Object[code.stacksize];
        Object result = evaluate(code.code);
        self = backup_self;
        variables[Levels.ARGS] = backup_args;
        variables[Levels.BODY] = backup_body;

        // !!! System.out.println("!!! RETURN");
        // !!! System.out.println("!!! code.symbol    = " +Debug.show(code.symbol));
        // !!! System.out.println("!!! result.class   = " + Debug.show(result));

        return result;
        } catch (EvaluatorException exception) {
            throw exception;
        } catch (Error exception) {
            throw exception;
        } catch (Throwable exception) {
            throw Debug.abort(exception);
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

        case Invoke(Code target, Function function, Code[] parameters, int pos, Symbol method):
            Object object = evaluate(target);
            Object[] args = new Object[parameters.length];
            for (int i = 0; i < args.length; i++) {
                args[i] = evaluate(parameters[i]);
            }
            try { // !!! indent
                return invoke(object, function, args);
            } catch (EvaluatorException exception) {
                exception.addCall(pos, method);
                throw exception;
            }

        case Create(ScalaTemplate template):
            return invoke(null, template.getConstructor(),
                new Object[] {template.getHandler()});

        case Box(Code value):
            return box(evaluate(value));

        case IsScala(Code target, Symbol symbol):
            Object object = evaluate(target);
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
            // !!! System.out.println("!!! Self: " + Debug.show(self));
            return self;

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
            // !!! handle case where object is null
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
            throw new EvaluatorException((Throwable)object);

        case StringPlus:
            assert args.length == 1 : Debug.show(args);
            //assert object instanceof String : object.getClass().getName();
            return (String.valueOf(object)).concat(String.valueOf(args[0]));

        case EqEq:
            assert args.length == 1 : Debug.show(args);
            return object == null ? new Boolean(args[0] == null) : isScalaObject(object) ? getScalaObject(object).invoke(object, scalac.Global.instance.definitions.EQEQ, args) : new Boolean(object.equals(args[0])); // !!!

        case BangEq:
            assert args.length == 1 : Debug.show(args);
            return object == null ? new Boolean(args[0] != null) : isScalaObject(object) ? getScalaObject(object).invoke(object, scalac.Global.instance.definitions.BANGEQ, args) : new Boolean(!object.equals(args[0])); // !!!

        case HashCode:
            assert args.length == 0 : Debug.show(args);
            return new Integer(getScalaObject(object).hashCode());

        case ToString:
            assert args.length == 0 : Debug.show(args);
            return getScalaObject(object).toString();

        default:
            throw Debug.abort("illegal function", function);
        }
    }

    private Object invoke(Object object, Constructor constructor,Object[]args){
        try {
            // !!! System.out.println("!!! object " + (object == null ? "null" : object.getClass().getName()));
            // !!! System.out.println("!!! constr " + constructor);
            // !!! System.out.println("!!! nbargs " + args.length);
            return constructor.newInstance(args);
        } catch (ExceptionInInitializerError exception) {
            return exception(exception.getException());
        } catch (InvocationTargetException exception) {
            return exception(exception.getTargetException());
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
        } catch (NullPointerException exception) {
            return exception(exception);
        } catch (ExceptionInInitializerError exception) {
            return exception(exception.getException());
        } catch (InvocationTargetException exception) {
            return exception(exception.getTargetException());
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

    // !!! return void ?
    private Object store(Object object, Variable variable, Object value) {
        switch (variable) {

        case Global(_):
            return ((Variable.Global)variable).value = value;

        case Context(int level, int index):
            // !!! System.out.println("!!! store Context: level = " + level + ", index = " + index);
            return variables[level][index] = value;

        case Member(int index):
            // !!! handle case where object is null
            // !!! System.out.println("!!! store Member: index = " + index + ", length = " + getScalaObject(object).variables.length);
            return getScalaObject(object).variables[index] = value;

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
            return exception(exception);
        } catch (ExceptionInInitializerError exception) {
            return exception(exception.getException());
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

        case Context(int level, int index):
            return variables[level][index];

        case Module(CodePromise body, Object value):
            if (value == null) {
                // !!!
                ((Variable.Module)variable).value = value = evaluate(body.force().code);
                ((Variable.Module)variable).body = null;
            }
            return value;

        case Member(int index):
            // !!! handle case where object is null
            // !!! System.out.println("!!! loadScala: self  = " + Debug.show(object) + ", index = " + index);
            // !!! System.out.println("!!! loadScala: value = " + Debug.show(getScalaObject(object).variables[index]));
            return getScalaObject(object).variables[index];

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
            return exception(exception);
        } catch (ExceptionInInitializerError exception) {
            return exception(exception.getException());
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
    // Private Methods - box

    // !!!
    private final Class Boolean_class   = Boolean.class;
    private final Class Byte_class      = Byte.class;
    private final Class Short_class     = Short.class;
    private final Class Character_class = Character.class;
    private final Class Integer_class   = Integer.class;
    private final Class Long_class      = Long.class;
    private final Class Float_class     = Float.class;
    private final Class Double_class    = Double.class;

    // !!!
    private final Class boolean_class   = Boolean.TYPE;
    private final Class byte_class      = Byte.TYPE;
    private final Class short_class     = Short.TYPE;
    private final Class char_class      = Character.TYPE;
    private final Class int_class       = Integer.TYPE;
    private final Class long_class      = Long.TYPE;
    private final Class float_class     = Float.TYPE;
    private final Class double_class    = Double.TYPE;

    private Object box(Object value) {
        // !!! check
        // !!! System.out.println("!!! box: ");
        if (value == null) return null;
        Class type = value.getClass();
        if (type == Boolean_class)
            return RunTime.box(((Boolean)value).booleanValue());
        if (type == Byte_class)
            return RunTime.box(((Byte)value).byteValue());
        if (type == Short_class)
            return RunTime.box(((Short)value).shortValue());
        if (type == Character_class)
            return RunTime.box(((Character)value).charValue());
        if (type == Integer_class)
            return RunTime.box(((Integer)value).intValue());
        if (type == Long_class)
            return RunTime.box(((Long)value).longValue());
        if (type == Float_class)
            return RunTime.box(((Float)value).floatValue());
        if (type == Double_class)
            return RunTime.box(((Double)value).doubleValue());
        if (type.isArray()) {
            type = type.getComponentType();
            if (type == boolean_class) return RunTime.box((boolean[])value);
            if (type == byte_class) return RunTime.box((byte[])value);
            if (type == short_class) return RunTime.box((short[])value);
            if (type == char_class) return RunTime.box((char[])value);
            if (type == int_class) return RunTime.box((int[])value);
            if (type == long_class) return RunTime.box((long[])value);
            if (type == float_class) return RunTime.box((float[])value);
            if (type == double_class) return RunTime.box((double[])value);
            return RunTime.box((Object[])value);
        }
        return value;
    }

    //########################################################################
    // Private Methods - !!!

    private boolean isScalaObject(Object object) {
        return Proxy.isProxyClass(object.getClass()); // !!! add more checks ?
    }

    private ScalaObject getScalaObject(Object object) {
        if (object == null)
            return (ScalaObject)exception(new NullPointerException()); // !!!
        assert object instanceof Proxy : object.getClass();
        return (ScalaObject)Proxy.getInvocationHandler(object);
    }

    private Object exception(Throwable exception) {
        // !!! return an exception result
        if (exception instanceof EvaluatorException) {
            throw (EvaluatorException)exception;
        }
        if (exception instanceof Error) {
            throw (Error)exception;
        }
        throw new EvaluatorException(exception, new Error());
    }

    // !!! remove ?
    private Error abort(Exception exception) {
        // !!! return an abort result ?
        return Debug.abort(exception);
    }

    //########################################################################
}
