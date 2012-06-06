package scala.reflect

import language.experimental.macros

package object runtime {

  // type is api.JavaUniverse because we only want to expose the `scala.reflect.api.*` subset of reflection
  lazy val universe: api.JavaUniverse = new runtime.JavaUniverse

  // [Eugene++ to Martin] removed `mirrorOfLoader`, because one can use `universe.runtimeMirror` instead

  def currentMirror: universe.Mirror = ???
}
