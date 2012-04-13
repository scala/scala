package scala

import annotation.meta

object languageFeature {

  @meta.languageFeature("extension of type scala.Dynamic", true)
  sealed trait dynamics

  @meta.languageFeature("postfix operator #", false)
  sealed trait postfixOps

  @meta.languageFeature("reflective access of structural type member #", false)
  sealed trait reflectiveCalls

  @meta.languageFeature("implicit conversion #", false)
  sealed trait implicitConversions

  @meta.languageFeature("higher-kinded type", false)
  sealed trait higherKinds

  @meta.languageFeature("#, which cannot be expressed by wildcards, ", false)
  sealed trait existentials

  object experimental {
    @meta.languageFeature("macro definition", true)
    sealed trait macros
  }
}

