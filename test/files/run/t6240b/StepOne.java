import java.io.File;
import java.io.IOException;
import java.lang.ClassNotFoundException;
import java.lang.NoSuchMethodException;
import java.lang.IllegalAccessException;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.net.URLClassLoader;
import java.net.MalformedURLException;

public class StepOne {
  public static void main(String[] args)
  throws MalformedURLException, ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException, IOException {
    String[] launchPaths = System.getProperty("launch.classpath").split(File.pathSeparator);

    // move away StepThree
    File tempDir = File.createTempFile("temp", Long.toString(System.nanoTime()));
    System.setProperty("launch.step.three", tempDir.getAbsolutePath());
    tempDir.delete();
    tempDir.mkdir();
    File[] testClasses = new File(launchPaths[0]).listFiles();
    for (int i = 0; i < testClasses.length; i++) {
      File testClass = testClasses[i];
      if (testClass.getPath().contains("StepThree")) {
        File testClassMoved = new File(tempDir.getAbsolutePath() + "/" + testClass.getName());
        testClass.renameTo(testClassMoved);
      }
    }

    // launch StepTwo
    URL[] launchURLs = new URL[launchPaths.length];
    for (int i = 0; i < launchPaths.length; i++) {
      launchURLs[i] = new File(launchPaths[i]).toURL();
    }
    URLClassLoader classLoader = new URLClassLoader(launchURLs, Object.class.getClassLoader());
    Class<?> stepTwo = classLoader.loadClass("StepTwo");
    Method main = stepTwo.getDeclaredMethod("main", String[].class);
    main.invoke(null, (Object)(new String[]{}));
  }
}
