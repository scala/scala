package scala

import annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", enableRequired = true)
  sealed trait dynamics

  @meta.languageFeature("postfix operator #", enableRequired = false)
  sealed trait postfixOps

  @meta.languageFeature("reflective access of structural type member #", enableRequired = false)
  sealed trait reflectiveCalls

  @meta.languageFeature("implicit conversion #", enableRequired = false)
  sealed trait implicitConversions

  @meta.languageFeature("higher-kinded type", enableRequired = false)
  sealed trait higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards, ", enableRequired = false)
  sealed trait existentials

  object experimental {
    @meta.languageFeature("macro definition", enableRequired = true)
    sealed trait macros
  }
}

