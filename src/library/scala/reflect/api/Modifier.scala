package scala.reflect.api

object Modifier extends Enumeration {

   val `protected`, `private`, `override`, `abstract`, `final`,
        `sealed`, `implicit`, `lazy`, `macro`, `case`, `trait`,
        deferred, interface, mutable, parameter, covariant, contravariant,
        preSuper, abstractOverride, local, java, static, caseAccessor,
        defaultParameter, defaultInit, paramAccessor, bynameParameter = Value

}
