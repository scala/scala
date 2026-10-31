package scala.collection.mutable

import org.junit.Assert.assertTrue

import scala.reflect.runtime.universe._

/** The range projections of `mutable.TreeMap` and `mutable.TreeSet` subclass the full collection and share its tree.
  * Methods the collection declares usually go straight to the tree and ignore the projection's bounds, so the
  * projection must override each of them unless it is known not to depend on the bounds.
  *
  * This catches a method added to the collection class without a corresponding override in its projection.
  */
object RangeProjectionOverrides {
  def assertOverridden(tpe: Type, projectionName: String, boundsAgnostic: Map[String, String]): Unit = {
    val cls = tpe.typeSymbol.asClass
    val projection = cls.info.decl(TypeName(projectionName)).asClass
    val overridden = projection.info.decls.toList.collect { case m: MethodSymbol => m.overrides }.flatten.toSet
    val candidates = cls.info.decls.toList.collect {
      case m: MethodSymbol if !m.isConstructor && !m.isPrivate && !m.isFinal => m
    }
    val names = candidates.map(_.name.decodedName.toString).toSet
    val missing = candidates.filterNot(overridden).map(_.name.decodedName.toString).filterNot(boundsAgnostic.contains).distinct.sorted
    assertTrue(
      s"${cls.name} declares ${missing.mkString(", ")} but $projectionName does not override them. " +
        "Override them to respect the projection's bounds, or add them to the bounds-agnostic list in the test.",
      missing.isEmpty)
    val stale = boundsAgnostic.keySet.diff(names).toList.sorted
    assertTrue(s"Stale entries in the bounds-agnostic list: ${stale.mkString(", ")}", stale.isEmpty)
  }
}
