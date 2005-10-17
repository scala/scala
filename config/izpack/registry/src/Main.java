// File       : Main.java
// Project    : Scala
// Author(s)  : Stephane Micheloud (mics)
// Environment: JNI Registry 3.1.3, Sun JDK 1.4.2_09
// Version    : 01
// Created    : 25.09.2005/mics
// Modified   : -


import com.ice.jni.registry.NoSuchKeyException;
import com.ice.jni.registry.NoSuchValueException;
import com.ice.jni.registry.Registry;
import com.ice.jni.registry.RegistryException;
import com.ice.jni.registry.RegistryKey;
import com.ice.jni.registry.RegistryValue;
import com.ice.jni.registry.RegMultiStringValue;
import com.ice.jni.registry.RegStringValue;

public class Main {

    private static final String PATH       = "PATH";
    private static final String SCALA_HOME = "SCALA_HOME";
    private static final String SCALA_BIN  = "%" + SCALA_HOME + "%\\bin";

    private static void updateRegistry(String homePath, String fullName) throws RegistryException {
        // HKEY_CURRENT_USER\Environment
        RegistryKey envKey = Registry.openSubkey(
            Registry.HKEY_CURRENT_USER,
            "Environment",
            RegistryKey.ACCESS_ALL);
        if (homePath != null) {
            // set home directory
            RegStringValue data = new RegStringValue(envKey, SCALA_HOME, homePath);
            envKey.setValue(data);
            // update user path
            String path = envKey.getStringValue(PATH);
            if (path.indexOf(SCALA_BIN) < 0) {
            	int inx = path.lastIndexOf(";");
            	StringBuffer buf = new StringBuffer(path);
                if (0 < inx && inx < path.length()-1)
                    buf.append(";");
                buf.append(SCALA_BIN);
                data = new RegStringValue(envKey, PATH, buf.toString());
                envKey.setValue(data);
            }
        }
        else { // uninstall
            // remove home directory
            try {
                envKey.deleteValue(SCALA_HOME);
            }
            catch (NoSuchValueException e) {
                // do nothing
            }
            String path = envKey.getStringValue(PATH);
            int inx1 = path.indexOf(SCALA_BIN);
            if (inx1 >= 0) {
                int inx2 = path.indexOf(";", inx1);
                StringBuffer buf = new StringBuffer(path.substring(0, inx1));
                if (inx2 >= 0)
                    buf.append(path.substring(inx2));
                RegStringValue data = new RegStringValue(envKey, PATH, buf.toString());
                envKey.setValue(data);
            }
        }

        // HKEY_CURRENT_USER\Software
        RegistryKey softwareKey = Registry.openSubkey(
            Registry.HKEY_CURRENT_USER,
            "Software",
            RegistryKey.ACCESS_WRITE);
        if (homePath != null) {
            RegistryKey productKey = softwareKey.createSubKey(
                fullName,
                "java.lang.String",
                RegistryKey.ACCESS_WRITE);
            RegStringValue data = new RegStringValue(productKey, "Location", homePath);
            productKey.setValue(data);
        }
        else { // uninstall
            try {
            	softwareKey.deleteSubKey(fullName);
                String name = fullName;
                int inx = name.lastIndexOf("\\");
                while (inx != -1) {
                    String parentName = name.substring(0, inx);
                    RegistryKey key = softwareKey.openSubKey(parentName, RegistryKey.ACCESS_READ);
                    int n = key.getNumberSubkeys();
            	    if (n == 0) softwareKey.deleteSubKey(parentName);
            	    name = parentName;
            	    inx = name.lastIndexOf("\\");
            	}
            }
            catch (NoSuchKeyException e) {
                // do nothing
            }
        }
    }

    public static void main(String[] args) {
    	int argc = args.length;
    	if (argc != 1 && argc != 2) {
    	    System.out.println("Usage: java Main <version> [ <installpath> ]\n");
    	    System.out.println("\tversion number (e.g. 1.4.0.1)");
    	    System.out.println("\tinstall path   (e.g. c:\\\\Program Files\\\\Scala)");
    	    System.out.println();
    	    System.exit((argc > 0) ? 1 : 0);
    	}
        String homePath = (argc == 2) ? args[1] : null;   // e.g. "C:\\Program Files\\Scala"
        String fullName = "EPFL\\Scala\\" + args[0];      // e.g. "EPFL\\Scala\\1.4.0.1"
        try {
            updateRegistry(homePath, fullName);
        }
        catch (RegistryException e) {
            System.out.println(e.getMessage());
            System.exit(1);
        }
    }

}
