// scalac: -Xfatal-warnings -deprecation -Xsource:3
class C
package object foo extends C

package foo2 { object `package` extends C }
