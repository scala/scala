package scala.runtime;

import org.junit.Assert;
import org.junit.Test;

import java.io.Serializable;
import java.lang.invoke.*;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;

public final class LambdaDeserializerTest {
    private LambdaHost lambdaHost = new LambdaHost();

    @Test
    public void serializationPrivate() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByPrivateImplMethod();
        Assert.assertEquals(f1.apply(true), reconstitute(f1).apply(true));
    }

    @Test
    public void serializationStatic() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        Assert.assertEquals(f1.apply(true), reconstitute(f1).apply(true));
    }

    @Test
    public void serializationVirtualMethodReference() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByVirtualMethodReference();
        Assert.assertEquals(f1.apply(true), reconstitute(f1).apply(true));
    }

    @Test
    public void serializationInterfaceMethodReference() {
        F1<I, Object> f1 = lambdaHost.lambdaBackedByInterfaceMethodReference();
        I i = new I() {
        };
        Assert.assertEquals(f1.apply(i), reconstitute(f1).apply(i));
    }

    @Test
    public void serializationStaticMethodReference() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticMethodReference();
        Assert.assertEquals(f1.apply(true), reconstitute(f1).apply(true));
    }

    @Test
    public void serializationNewInvokeSpecial() {
        F0<Object> f1 = lambdaHost.lambdaBackedByConstructorCall();
        Assert.assertEquals(f1.apply(), reconstitute(f1).apply());
    }

    @Test
    public void uncached() {
        F0<Object> f1 = lambdaHost.lambdaBackedByConstructorCall();
        F0<Object> reconstituted1 = reconstitute(f1);
        F0<Object> reconstituted2 = reconstitute(f1);
        Assert.assertNotEquals(reconstituted1.getClass(), reconstituted2.getClass());
    }

    @Test
    public void cached() {
        HashMap<String, MethodHandle> cache = new HashMap<>();
        F0<Object> f1 = lambdaHost.lambdaBackedByConstructorCall();
        F0<Object> reconstituted1 = reconstitute(f1, cache);
        F0<Object> reconstituted2 = reconstitute(f1, cache);
        Assert.assertEquals(reconstituted1.getClass(), reconstituted2.getClass());
    }

    @Test
    public void cachedStatic() {
        HashMap<String, MethodHandle> cache = new HashMap<>();
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        // Check that deserialization of a static lambda always returns the
        // same instance.
        Assert.assertSame(reconstitute(f1, cache), reconstitute(f1, cache));

        // (as is the case with regular invocation.)
        Assert.assertSame(f1, lambdaHost.lambdaBackedByStaticImplMethod());
    }

    @Test
    public void implMethodNameChanged() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        SerializedLambda sl = writeReplace(f1);
        checkIllegalAccess(sl, copySerializedLambda(sl, sl.getImplMethodName() + "___", sl.getImplMethodSignature()));
    }

    @Test
    public void implMethodSignatureChanged() {
        F1<Boolean, String> f1 = lambdaHost.lambdaBackedByStaticImplMethod();
        SerializedLambda sl = writeReplace(f1);
        checkIllegalAccess(sl, copySerializedLambda(sl, sl.getImplMethodName(), sl.getImplMethodSignature().replace("Boolean", "Integer")));
    }

    private void checkIllegalAccess(SerializedLambda allowed, SerializedLambda requested) {
        try {
            HashMap<String, MethodHandle> allowedMap = createAllowedMap(LambdaHost.lookup(), allowed);
            LambdaDeserializer.deserializeLambda(MethodHandles.lookup(), null, allowedMap, requested);
            throw new AssertionError();
        } catch (IllegalArgumentException iae) {
            if (!iae.getMessage().contains("Illegal lambda deserialization")) {
                Assert.fail("Unexpected message: " + iae.getMessage());
            }
        }
    }

    private SerializedLambda copySerializedLambda(SerializedLambda sl, String implMethodName, String implMethodSignature) {
        Object[] captures = new Object[sl.getCapturedArgCount()];
        for (int i = 0; i < captures.length; i++) {
            captures[i] = sl.getCapturedArg(i);
        }
        return new SerializedLambda(loadClass(sl.getCapturingClass()), sl.getFunctionalInterfaceClass(), sl.getFunctionalInterfaceMethodName(),
                sl.getFunctionalInterfaceMethodSignature(), sl.getImplMethodKind(), sl.getImplClass(), implMethodName, implMethodSignature,
                sl.getInstantiatedMethodType(), captures);
    }

    private Class<?> loadClass(String className) {
        try {
            return Class.forName(className.replace('/', '.'));
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private <A, B> A reconstitute(A f1) {
        return reconstitute(f1, null);
    }

    @SuppressWarnings("unchecked")
    private <A, B> A reconstitute(A f1, java.util.HashMap<String, MethodHandle> cache) {
        try {
            return deserizalizeLambdaCreatingAllowedMap(f1, cache, LambdaHost.lookup());
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private <A> A deserizalizeLambdaCreatingAllowedMap(A f1, HashMap<String, MethodHandle> cache, MethodHandles.Lookup lookup) {
        SerializedLambda serialized = writeReplace(f1);
        HashMap<String, MethodHandle> allowed = createAllowedMap(lookup, serialized);
        return (A) LambdaDeserializer.deserializeLambda(lookup, cache, allowed, serialized);
    }

    private HashMap<String, MethodHandle> createAllowedMap(MethodHandles.Lookup lookup, SerializedLambda serialized) {
        Class<?> implClass = classForName(serialized.getImplClass().replace("/", "."), lookup.lookupClass().getClassLoader());
        MethodHandle implMethod = findMember(lookup, serialized.getImplMethodKind(), implClass, serialized.getImplMethodName(), MethodType.fromMethodDescriptorString(serialized.getImplMethodSignature(), lookup.lookupClass().getClassLoader()));
        HashMap<String, MethodHandle> allowed = new HashMap<>();
        allowed.put(LambdaDeserialize.nameAndDescriptorKey(serialized.getImplMethodName(), serialized.getImplMethodSignature()), implMethod);
        return allowed;
    }

    private Class<?> classForName(String className, ClassLoader classLoader) {
        try {
            return Class.forName(className, true, classLoader);
        } catch (ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    private MethodHandle findMember(MethodHandles.Lookup lookup, int kind, Class<?> owner,
                                    String name, MethodType signature) {
        try {
            switch (kind) {
                case MethodHandleInfo.REF_invokeStatic:
                    return lookup.findStatic(owner, name, signature);
                case MethodHandleInfo.REF_newInvokeSpecial:
                    return lookup.findConstructor(owner, signature);
                case MethodHandleInfo.REF_invokeVirtual:
                case MethodHandleInfo.REF_invokeInterface:
                    return lookup.findVirtual(owner, name, signature);
                case MethodHandleInfo.REF_invokeSpecial:
                    return lookup.findSpecial(owner, name, signature, owner);
                default:
                    throw new IllegalArgumentException();
            }
        } catch (NoSuchMethodException | IllegalAccessException e) {
            throw new RuntimeException(e);
        }
    }


    private <A> SerializedLambda writeReplace(A f1) {
        try {
            Method writeReplace = f1.getClass().getDeclaredMethod("writeReplace");
            writeReplace.setAccessible(true);
            return (SerializedLambda) writeReplace.invoke(f1);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}


interface F1<A, B> extends Serializable {
    B apply(A a);
}

interface F0<A> extends Serializable {
    A apply();
}

class LambdaHost {
    public F1<Boolean, String> lambdaBackedByPrivateImplMethod() {
        int local = 42;
        return (b) -> Arrays.asList(local, b ? "true" : "false", LambdaHost.this).toString();
    }

    @SuppressWarnings("Convert2MethodRef")
    public F1<Boolean, String> lambdaBackedByStaticImplMethod() {
        return (b) -> String.valueOf(b);
    }

    public F1<Boolean, String> lambdaBackedByStaticMethodReference() {
        return String::valueOf;
    }

    public F1<Boolean, String> lambdaBackedByVirtualMethodReference() {
        return Object::toString;
    }

    public F1<I, Object> lambdaBackedByInterfaceMethodReference() {
        return I::i;
    }

    public F0<Object> lambdaBackedByConstructorCall() {
        return String::new;
    }

    public static MethodHandles.Lookup lookup() {
        return MethodHandles.lookup();
    }
}

interface I {
    default String i() {
        return "i";
    }
}
