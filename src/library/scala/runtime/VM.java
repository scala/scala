package scala.runtime;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Field;

public class VM {
    public static final MethodHandle RELEASE_FENCE;


    static {
        RELEASE_FENCE = mkHandle();
    }

    private static MethodHandle mkHandle() {
        MethodHandles.Lookup lookup = MethodHandles.lookup();
        try {
            return lookup.findStatic(Class.forName("java.lang.invoke.VarHandle"), "releaseFence", MethodType.methodType(Void.TYPE));
        } catch (ClassNotFoundException e) {
            try {
                Class<?> unsafeClass = Class.forName("sun.misc.Unsafe");
                return lookup.findVirtual(unsafeClass, "storeFence", MethodType.methodType(void.class)).bindTo(findUnsafe(unsafeClass));
            } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException e1) {
                ExceptionInInitializerError error = new ExceptionInInitializerError(e1);
                error.addSuppressed(e);
                throw error;
            }
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private static Object findUnsafe(Class<?> unsafeClass) throws IllegalAccessException {
        Object found = null;
        for (Field field : unsafeClass.getDeclaredFields()) {
            if (field.getType() == unsafeClass) {
                field.setAccessible(true);
                found = field.get(null);
                break;
            }
        }
        if (found == null) throw new IllegalStateException("No instance of Unsafe found");
        return found;
    }
}
