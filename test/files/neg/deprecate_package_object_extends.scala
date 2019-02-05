// scalac: -Xfatal-warnings -deprecation
class C
package object foo extends C

package foo2 { object `package` extends C }
