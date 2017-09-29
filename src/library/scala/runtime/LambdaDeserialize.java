package scala.runtime;


import java.lang.invoke.*;
import java.util.HashMap;

public final class LambdaDeserialize {
    public static final MethodType DESERIALIZE_LAMBDA_MT = MethodType.fromMethodDescriptorString("(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", LambdaDeserialize.class.getClassLoader());

    private MethodHandles.Lookup lookup;
    private final HashMap<String, MethodHandle> cache = new HashMap<>();
    private final HashMap<String, MethodHandle> targetMethodMap;
    private static final MethodHandle LAMBDA_DESERIALIZER_DESERIALIZE_LAMBDA;

    static {
        LAMBDA_DESERIALIZER_DESERIALIZE_LAMBDA = lookup();
    }
    private static MethodHandle lookup() {
        try {
            return MethodHandles.lookup().findStatic(Class.forName("scala.runtime.LambdaDeserializer"), "deserializeLambda", MethodType.methodType(Object.class, MethodHandles.Lookup.class, java.util.Map.class, java.util.Map.class, SerializedLambda.class));
        } catch (NoSuchMethodException | ClassNotFoundException | IllegalAccessException e) {
            throw new ExceptionInInitializerError(e);
        }
    }

    private LambdaDeserialize(MethodHandles.Lookup lookup, MethodHandle[] targetMethods) {
        this.lookup = lookup;
        targetMethodMap = new HashMap<>(targetMethods.length);
        for (MethodHandle targetMethod : targetMethods) {
            MethodHandleInfo info = lookup.revealDirect(targetMethod);
            String key = nameAndDescriptorKey(info.getName(), info.getMethodType().toMethodDescriptorString());
            targetMethodMap.put(key, targetMethod);
        }
    }


    public Object deserializeLambda(SerializedLambda serialized) throws Throwable {
        return LAMBDA_DESERIALIZER_DESERIALIZE_LAMBDA.invoke(lookup, cache, targetMethodMap, serialized);
    }

    public static CallSite bootstrap(MethodHandles.Lookup lookup, String invokedName,
                                     MethodType invokedType, MethodHandle... targetMethods) throws Throwable {
        MethodHandle deserializeLambda = lookup.findVirtual(LambdaDeserialize.class, "deserializeLambda", DESERIALIZE_LAMBDA_MT);
        MethodHandle exact = deserializeLambda.bindTo(new LambdaDeserialize(lookup, targetMethods)).asType(invokedType);
        return new ConstantCallSite(exact);
    }
    public static String nameAndDescriptorKey(String name, String descriptor) {
        return name + descriptor;
    }
}
