// [Eugene++] delete this once we merge with trunk and have a working IDE

package scala.reflect {
  trait ErasureTag[T]
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
  import scala.reflect.makro.Context

  object internal_compat {
    private[scala] def materializeErasureTag_impl[T: c.TypeTag](c: Context)(u: c.Expr[BaseUniverse]): c.Expr[ErasureTag[T]] = ???
  }

  trait internal_compat {
    private[scala] def materializeErasureTag[T](u: BaseUniverse): ErasureTag[T] = macro internal_compat.materializeErasureTag_impl[T]
  }
}