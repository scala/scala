package scala.tools.nsc.profile;

/**
 * this is an external tool hook
 * it allows an external tool such as YourKit or JProfiler to instrument a particular phase of the compiler
 *
 * the use case is like this
 * other profiling has indicated that a particular phase of the compiler requires some deep analysis via an external tool
 *
 * confilerure the tool to start and stop profiling base on execution of these methods
 * then add the -YProfile-external-tool to call these methods for the pahse that you are interested in
 */
public class ExternalToolHook {
    private ExternalToolHook() {}
    public static void before() {}
    public static void after() {}
}
