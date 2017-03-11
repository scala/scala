package xsbti;

public interface ConsoleFactory {
  ConsoleInterface createConsole(String[] args, String bootClasspathString,
    String classpathString, String initialCommands, String cleanupCommands,
    ClassLoader loader, String[] bindNames, Object[] bindValues,
    Logger log);
}
