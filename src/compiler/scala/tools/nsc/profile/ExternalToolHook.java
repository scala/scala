/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc.profile;

/**
 * This is an external tool hook, it allows an external tool such as YourKit or JProfiler to instrument a
 * particular phase of the compiler.  
 * Profilers have hooks to allow starting and stopping profiling on a given method invocation.

 * To use add -Yprofile-external-tool (defaults to typer) or -Yprofile-external-tool:<phase> (for a specific compiler phase)
 * to the compiler flags.
 *
 * 'before' will be called at the start of the target phase and 'after' at the end, allowing custom profiling to be 
 * triggered. 
 */
public class ExternalToolHook {
    private ExternalToolHook() {}
    public static void before() {}
    public static void after() {}
}
