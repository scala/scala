package scala.tools.nsc

/** A helper trait to initialize things that need to be set before JavaMirrors and other
 *  reflect specific traits are initialized */
private[nsc] trait ReflectSetup { this: Global =>
  phase = new Run().typerPhase
}