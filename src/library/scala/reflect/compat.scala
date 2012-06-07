// [Eugene++] delete this once we merge with trunk and have a working IDE

package scala.reflect {
  trait ArrayTag[T]
  trait ErasureTag[T]
  trait ConcreteTypeTag[T]
}

package scala.reflect.api {
  trait TypeTags {
    trait TypeTag[T]
    trait ConcreteTypeTag[T]
  }
}

package scala {
  import scala.reflect.base.{Universe => BaseUniverse}

  trait reflect_compat {
    lazy val mirror: BaseUniverse = ???
  }
}

package scala.reflect {
  import language.experimental.macros
  import scala.reflect.base.{Universe => BaseUniverse}

  trait internal_compat {
    private[scala] def materializeArrayTag[T](u: BaseUniverse): ArrayTag[T] = ???
    private[scala] def materializeErasureTag[T](u: BaseUniverse): ErasureTag[T] = ???
    private[scala] def materializeConcreteTypeTag[T](u: BaseUniverse): ConcreteTypeTag[T] = ???
  }
}