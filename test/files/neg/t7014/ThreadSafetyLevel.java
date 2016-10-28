package t7014; // package needed due to other bug in scalac's java parser

// since we parse eagerly, we have not yet parsed the classfile when parsing the annotation,
// and on doing so, fail to find a symbol for the COMPLETELY_THREADSAFE reference
// from the annotation's argument to the enum's member
// for now, let's just not crash -- should implement lazy completing at some point
@ThreadSafety(level=ThreadSafetyLevel.COMPLETELY_THREADSAFE)
public enum ThreadSafetyLevel { COMPLETELY_THREADSAFE }
