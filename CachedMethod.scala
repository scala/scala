package xsbt

import java.lang.ref.WeakReference
import java.lang.reflect.Method
import scala.reflect.Manifest

// replacement for structural type cache, which doesn't use weak references
// assumes type of target doesn't change
// not thread safe
private final class CachedMethod[T](name: String, tpes: Class[_]*)(mf: Manifest[T]) extends NotNull
{
	private var method = new WeakReference[Method](null)
	private def getMethod(on: AnyRef): Method =
	{
		val m = on.getClass.getMethod(name, tpes : _*)
		method = new WeakReference(m)
		m
	}
	def force(on: AnyRef) { getMethod(on) }
	def apply(on: AnyRef, args: AnyRef*): T =
	{
		val cached = method.get
		val m = if(cached ne null) cached else getMethod(on)
		val result = m.invoke(on, args : _*)
		mf.erasure.cast(result).asInstanceOf[T]
	}
}