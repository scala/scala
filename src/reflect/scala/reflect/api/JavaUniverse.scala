package scala.reflect
package api

/** A refinement of [[scala.reflect.api.Universe]] for runtime reflection using JVM classloaders.
 *
 *  The refinement consists of an upgrade to the mirror API, which gets extended from [[scala.reflect.api.Mirror]]
 *  to [[scala.reflect.api.JavaMirrors#JavaMirror]].
 *
 *  See the [[http://docs.scala-lang.org/overviews/reflection/overview.html Reflection Guide]] for details on how to use runtime reflection.
 *  @groupname JavaUniverse Java Mirrors
 *
 *  @contentDiagram hideNodes "*Api"
 */
trait JavaUniverse extends Universe with JavaMirrors { self =>

  /*  @group JavaUniverse */
  override def typeTagToManifest[T: ClassTag](mirror0: Any, tag: Universe # TypeTag[T]): Manifest[T] = {
    // SI-6239: make this conversion more precise
    val mirror = mirror0.asInstanceOf[Mirror]
    val runtimeClass = mirror.runtimeClass(tag.in(mirror).tpe)
    Manifest.classType(runtimeClass).asInstanceOf[Manifest[T]]
  }

  /*  @group JavaUniverse */
  override def manifestToTypeTag[T](mirror0: Any, manifest: Manifest[T]): Universe # TypeTag[T] =
    TypeTag(mirror0.asInstanceOf[Mirror], new TypeCreator {
      def apply[U <: Universe with Singleton](mirror: scala.reflect.api.Mirror[U]): U # Type = {
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
