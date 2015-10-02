package scala.runtime

import java.lang.invoke._

/**
 * This class is only intended to be called by synthetic `$deserializeLambda$` method that the Scala 2.12
 * compiler will add to classes hosting lambdas.
 *
 * It is not intended to be consumed directly.
 */
object LambdaDeserializer {
  /**
   * Deserialize a lambda by calling `LambdaMetafactory.altMetafactory` to spin up a lambda class
   * and instantiating this class with the captured arguments.
   *
   * A cache may be provided to ensure that subsequent deserialization of the same lambda expression
   * is cheap, it amounts to a reflective call to the constructor of the previously created class.
   * However, deserialization of the same lambda expression is not guaranteed to use the same class,
   * concurrent deserialization of the same lambda expression may spin up more than one class.
   *
   * Assumptions:
   *  - No additional marker interfaces are required beyond `{java.io,scala.}Serializable`. These are
   *    not stored in `SerializedLambda`, so we can't reconstitute them.
   *  - No additional bridge methods are passed to `altMetafactory`. Again, these are not stored.
   *
   * @param lookup      The factory for method handles. Must have access to the implementation method, the
   *                    functional interface class, and `java.io.Serializable` or `scala.Serializable` as
   *                    required.
   * @param cache       A cache used to avoid spinning up a class for each deserialization of a given lambda. May be `null`
   * @param serialized  The lambda to deserialize. Note that this is typically created by the `readResolve`
   *                    member of the anonymous class created by `LambdaMetaFactory`.
   * @return            An instance of the functional interface
   */
  def deserializeLambda(lookup: MethodHandles.Lookup, cache: java.util.Map[String, MethodHandle], serialized: SerializedLambda): AnyRef = {
    def slashDot(name: String) = name.replaceAll("/", ".")
    val loader = lookup.lookupClass().getClassLoader
    val implClass = loader.loadClass(slashDot(serialized.getImplClass))

    def makeCallSite: CallSite = {
      import serialized._
      def parseDescriptor(s: String) =
        MethodType.fromMethodDescriptorString(s, loader)

      val funcInterfaceSignature = parseDescriptor(getFunctionalInterfaceMethodSignature)
      val instantiated = parseDescriptor(getInstantiatedMethodType)
      val functionalInterfaceClass = loader.loadClass(slashDot(getFunctionalInterfaceClass))

      val implMethodSig = parseDescriptor(getImplMethodSignature)
      // Construct the invoked type from the impl method type. This is the type of a factory
      // that will be generated by the meta-factory. It is a method type, with param types
      // coming form the types of the captures, and return type being the functional interface.
      val invokedType: MethodType = {
        // 1. Add receiver for non-static impl methods
        val withReceiver = getImplMethodKind match {
          case MethodHandleInfo.REF_invokeStatic | MethodHandleInfo.REF_newInvokeSpecial =>
            implMethodSig
          case _ =>
            implMethodSig.insertParameterTypes(0, implClass)
        }
        // 2. Remove lambda parameters, leaving only captures. Note: the receiver may be a lambda parameter,
        //    such as in `Function<Object, String> s = Object::toString`
        val lambdaArity = funcInterfaceSignature.parameterCount()
        val from = withReceiver.parameterCount() - lambdaArity
        val to = withReceiver.parameterCount()

        // 3. Drop the lambda return type and replace with the functional interface.
        withReceiver.dropParameterTypes(from, to).changeReturnType(functionalInterfaceClass)
      }

      // Lookup the implementation method
      val implMethod: MethodHandle = try {
        findMember(lookup, getImplMethodKind, implClass, getImplMethodName, implMethodSig)
      } catch {
        case e: ReflectiveOperationException => throw new IllegalArgumentException("Illegal lambda deserialization", e)
      }

      val flags: Int = LambdaMetafactory.FLAG_SERIALIZABLE | LambdaMetafactory.FLAG_MARKERS
      val isScalaFunction = functionalInterfaceClass.getName.startsWith("scala.Function")
      val markerInterface: Class[_] = loader.loadClass(if (isScalaFunction) ScalaSerializable else JavaIOSerializable)

      LambdaMetafactory.altMetafactory(
        lookup, getFunctionalInterfaceMethodName, invokedType,

        /* samMethodType          = */ funcInterfaceSignature,
        /* implMethod             = */ implMethod,
        /* instantiatedMethodType = */ instantiated,
        /* flags                  = */ flags.asInstanceOf[AnyRef],
        /* markerInterfaceCount   = */ 1.asInstanceOf[AnyRef],
        /* markerInterfaces[0]    = */ markerInterface,
        /* bridgeCount            = */ 0.asInstanceOf[AnyRef]
      )
    }

    val key = serialized.getImplMethodName + " : " + serialized.getImplMethodSignature
    val factory: MethodHandle = if (cache == null) {
      makeCallSite.getTarget
    } else cache.get(key) match {
      case null =>
        val callSite = makeCallSite
        val temp = callSite.getTarget
        cache.put(key, temp)
        temp
      case target => target
    }

    val captures = Array.tabulate(serialized.getCapturedArgCount)(n => serialized.getCapturedArg(n))
    factory.invokeWithArguments(captures: _*)
  }

  private val ScalaSerializable = "scala.Serializable"

  private val JavaIOSerializable = {
    // We could actually omit this marker interface as LambdaMetaFactory will add it if
    // the FLAG_SERIALIZABLE is set and of the provided markers extend it. But the code
    // is cleaner if we uniformly add a single marker, so I'm leaving it in place.
    "java.io.Serializable"
  }

  private def findMember(lookup: MethodHandles.Lookup, kind: Int, owner: Class[_],
                         name: String, signature: MethodType): MethodHandle = {
    kind match {
      case MethodHandleInfo.REF_invokeStatic =>
        lookup.findStatic(owner, name, signature)
      case MethodHandleInfo.REF_newInvokeSpecial =>
        lookup.findConstructor(owner, signature)
      case MethodHandleInfo.REF_invokeVirtual | MethodHandleInfo.REF_invokeInterface =>
        lookup.findVirtual(owner, name, signature)
      case MethodHandleInfo.REF_invokeSpecial =>
        lookup.findSpecial(owner, name, signature, owner)
    }
  }
}
