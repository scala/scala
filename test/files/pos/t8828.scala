
package outer

package inner {

  private[inner] class A

  // the class is final: no warning
  private[outer] final class B {
    def doWork(a: A): A = a
  }

  // the trait is sealed and doWork is not 
  // and cannot be overridden: no warning
  private[outer] sealed trait C {
    def doWork(a: A): A = a
  }

  private[outer] final class D extends C
}
