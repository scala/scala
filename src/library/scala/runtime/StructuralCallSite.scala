package scala.runtime

import java.lang.invoke._
import java.lang.ref.SoftReference
import java.lang.reflect.Method

final class StructuralCallSite private (callType: MethodType) {
  private var cache: SoftReference[MethodCache] =  new SoftReference(new EmptyMethodCache)

  val parameterTypes: Array[Class[_]] = callType.parameterArray

  def get: MethodCache = {
    var cache = this.cache.get
    if (cache == null) {
      cache = new EmptyMethodCache
      this.cache = new SoftReference(cache)
    }
    cache
  }

  def find(receiver: Class[_]): Method = get.find(receiver)

  def add(receiver: Class[_], m: Method): Method = {
    cache = new SoftReference(get.add(receiver, m))
    m
  }
}

object StructuralCallSite {
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, reflectiveCallType: MethodType): CallSite = {
    val structuralCallSite = new StructuralCallSite(reflectiveCallType)
    new ConstantCallSite(MethodHandles.constant(classOf[StructuralCallSite], structuralCallSite))
  }
}
