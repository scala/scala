// javaVersion: 17+
package p;

sealed interface S permits A, T {}

final class A implements S {}

final class B implements T {}
