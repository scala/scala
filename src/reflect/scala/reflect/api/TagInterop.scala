package scala.reflect
package api

import scala.reflect.base.TypeCreator
import scala.reflect.base.{Universe => BaseUniverse}

trait TagInterop { self: JavaUniverse =>

  override def typeTagToManifest[T: ClassTag](mirror0: Any, tag: base.Universe # TypeTag[T]): Manifest[T] = {
    // SI-6239: make this conversion more precise
    val mirror = mirror0.asInstanceOf[Mirror]
    val runtimeClass = mirror.runtimeClass(tag.in(mirror).tpe)
    Manifest.classType(runtimeClass).asInstanceOf[Manifest[T]]
  }

  override def manifestToTypeTag[T](mirror0: Any, manifest: Manifest[T]): base.Universe # TypeTag[T] =
    TypeTag(mirror0.asInstanceOf[Mirror], new TypeCreator {
      def apply[U <: BaseUniverse with Singleton](mirror: MirrorOf[U]): U # Type = {
        mirror.universe match {
          case ju: JavaUniverse =>
            val jm = mirror.asInstanceOf[ju.Mirror]
            val sym = jm.classSymbol(manifest.erasure)
            val tpe =
              if (manifest.typeArguments.isEmpty) sym.toType
              else ju.appliedType(sym.toTypeConstructor, manifest.typeArguments map (targ => ju.manifestToTypeTag(jm, targ)) map (_.in(jm).tpe))
            tpe.asInstanceOf[U # Type]
          case u =>
            u.manifestToTypeTag(mirror.asInstanceOf[u.Mirror], manifest).in(mirror).tpe
        }
      }
    })
}
