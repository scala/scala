package scala.reflect
package internal

import scala.reflect.base.{TreeCreator, TypeCreator}
import scala.reflect.base.{Universe => BaseUniverse}

trait StdCreators {
  self: SymbolTable =>

  case class FixedMirrorTreeCreator(mirror: MirrorOf[StdCreators.this.type], tree: Tree) extends TreeCreator {
    def apply[U <: BaseUniverse with Singleton](m: MirrorOf[U]): U # Tree =
      if (m eq mirror) tree.asInstanceOf[U # Tree]
      else throw new IllegalArgumentException(s"Expr defined in $mirror cannot be migrated to other mirrors.")
  }

  case class FixedMirrorTypeCreator(mirror: MirrorOf[StdCreators.this.type], tpe: Type) extends TypeCreator {
    def apply[U <: BaseUniverse with Singleton](m: MirrorOf[U]): U # Type =
      if (m eq mirror) tpe.asInstanceOf[U # Type]
      else throw new IllegalArgumentException(s"Type tag defined in $mirror cannot be migrated to other mirrors.")
  }
}