package scala.runtime;


import java.lang.invoke.*;
import java.lang.reflect.Field;
import java.util.Objects;

import static java.lang.invoke.MethodHandles.*;
import static java.lang.invoke.MethodType.methodType;

public final class ModuleLoad {
    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType, Class<?> moduleClass) throws Throwable {
        MethodHandle getter = lookup.findStaticGetter(moduleClass, "MODULE$", moduleClass);
        return new ModuleLoadCallSite(getter);
    }

    private static final class ModuleLoadCallSite extends MutableCallSite {
        private MethodHandle getter;
        private static final MethodHandle FALLBACK;

        static {
            MethodHandles.Lookup lookup = MethodHandles.lookup();
            try {
                FALLBACK =
                        lookup.findVirtual(
                                ModuleLoadCallSite.class, "fallback", methodType(MethodHandle.class));
            } catch (NoSuchMethodException | IllegalAccessException e) {
                throw new AssertionError(e);
            }
        }

        ModuleLoadCallSite(MethodHandle getter) {
            super(methodType(getter.type().returnType()));
            this.getter = getter;
            setTarget(foldArguments(exactInvoker(type()), FALLBACK.bindTo(this)));
        }

        private MethodHandle fallback() throws Throwable {
            Object result = getter.invoke();
            MethodHandle target = constant(getter.type().returnType(), result);
            if (result != null) {
                setTarget(target);
            }
            return target;
        }
    }
}
