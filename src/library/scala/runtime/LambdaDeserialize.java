package scala.runtime;


import java.lang.invoke.*;
import java.util.HashMap;

public final class LambdaDeserialize {
    public static final MethodType DESERIALIZE_LAMBDA_MT = MethodType.fromMethodDescriptorString("(Ljava/lang/invoke/SerializedLambda;)Ljava/lang/Object;", LambdaDeserialize.class.getClassLoader());

    private MethodHandles.Lookup lookup;
    private final HashMap<String, MethodHandle> cache = new HashMap<>();
    private final LambdaDeserializer$ l = LambdaDeserializer$.MODULE$;
    private final HashMap<String, MethodHandle> targetMethodMap;

    private LambdaDeserialize(MethodHandles.Lookup lookup, MethodHandle[] targetMethods) {
        this.lookup = lookup;
        targetMethodMap = new HashMap<>(targetMethods.length);
        for (MethodHandle targetMethod : targetMethods) {
            MethodHandleInfo info = lookup.revealDirect(targetMethod);
            String key = nameAndDescriptorKey(info.getName(), info.getMethodType().toMethodDescriptorString());
            targetMethodMap.put(key, targetMethod);
        }
    }

    public Object deserializeLambda(SerializedLambda serialized) {
        return l.deserializeLambda(lookup, cache, targetMethodMap, serialized);
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
