// javaVersion: 17+
package p;

// `S` is loaded from its classfile, whose `PermittedSubclasses` lists `T`
sealed interface T extends S permits B {}
