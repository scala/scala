package scala.runtime;


import java.lang.invoke.*;
import java.lang.ref.SoftReference;
import java.lang.reflect.Method;

public final class StructuralCallSite {

    private Class<?>[] _parameterTypes;
    private SoftReference<Object>/*<MethodCache>*/ cache = new SoftReference<>(newEmptyMethodCache());

    private static final MethodHandle EMPTY_METHOD_CACHE_INIT;
    private static final MethodHandle METHOD_CACHE_FIND;
    private static final MethodHandle METHOD_CACHE_ADD;

    static {
        EMPTY_METHOD_CACHE_INIT = lookupInit();
        METHOD_CACHE_FIND = lookupFind();
        METHOD_CACHE_ADD = lookupAdd();
    }

    private static MethodHandle lookupInit() {
        try {
            return MethodHandles.lookup().findConstructor(Class.forName("scala.runtime.EmptyMethodCache"), MethodType.methodType(Void.TYPE));
        } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }
    private static MethodHandle lookupFind() {
        try {
            return MethodHandles.lookup().findVirtual(Class.forName("scala.runtime.MethodCache"), "find", MethodType.methodType(Method.class, Class.class));
        } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }
    private static MethodHandle lookupAdd() {
        try {
            return MethodHandles.lookup().findVirtual(Class.forName("scala.runtime.MethodCache"), "add", MethodType.methodType(Class.forName("scala.runtime.MethodCache"), Class.class, Method.class));
        } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private StructuralCallSite(MethodType callType) {
        _parameterTypes = callType.parameterArray();
    }

    public Object get() {
        Object cache = this.cache.get();
        if (cache == null) {
            cache = newEmptyMethodCache();
            this.cache = new SoftReference<>(cache);
        }
        return cache;
    }

    private static Object newEmptyMethodCache() {
        try {
            return (Object) EMPTY_METHOD_CACHE_INIT.invoke();
        } catch (Throwable throwable) {
            throw new IllegalStateException(throwable);
        }
    }

    public Method find(Class<?> receiver) {
        try {
            return (Method) METHOD_CACHE_FIND.invoke(get(), receiver);
        } catch (Throwable throwable) {
            throw new IllegalStateException(throwable);
        }
    }

    public Method add(Class<?> receiver, Method m) {
        try {
            cache = new SoftReference<>(METHOD_CACHE_ADD.invoke(get(), receiver, m));
        } catch (Throwable throwable) {
            throw new IllegalStateException(throwable);
        }
        return m;
    }
    public Class<?>[] parameterTypes() {
        return _parameterTypes;
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType, MethodType reflectiveCallType) throws Throwable {
        StructuralCallSite structuralCallSite = new StructuralCallSite(reflectiveCallType);
        return new ConstantCallSite(MethodHandles.constant(StructuralCallSite.class, structuralCallSite));
    }
}
