package scala.tools.scalac4ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.util.*;
import java.io.File;
import java.util.*;


/** Scala AntTask.
 *
 * author: Burak Emir
 * adapted from package jaco.framework.ant.AntCompilerTask
 *  (part of Matthias Zenger's jaco framework)
 */


public class AntTask extends Javac {

    private String suffixes = "scala";
    private boolean force = false;
    private String source = null;
    private String target = null;
    private FileUtils fileUtils = FileUtils.newFileUtils();


    public void execute() {
        String old = project.getProperty("build.compiler");
        project.setProperty("build.compiler", "scala.tools.scalac4ant.AntAdaptor");
        super.execute();
        if (old == null)
        	project.setProperty("build.compiler", "modern");
        else
        	project.setProperty("build.compiler", old);
    }

    public void setForce(boolean fc) {
        force = fc;
    }

    public boolean getForce() {
        return force;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getSource() {
        return source;
    }

    public void setTarget(String target) {
        this.target = target;
    }

    public String getTarget() {
        return target;
    }

    public void setSuffixes(String s) {
        suffixes = s;
    }

    public String getSuffixes() {
        return suffixes;
    }

    protected String[] parseSuffixes() {
        StringTokenizer st = new StringTokenizer(suffixes, " ,");
        ArrayList al = new ArrayList();
        while (st.hasMoreTokens())
            al.add("." + st.nextToken());
        return (String[])al.toArray(new String[al.size()]);
    }

    private static boolean hasLegalSuffix(String[] suffixes, String file) {
        for (int i = 0; i < suffixes.length; i++)
            if (file.endsWith(suffixes[i]))
                return true;
        return false;
    }

    protected void scanDir(File srcDir, File destDir, String files[]) {
        String[] sfx = parseSuffixes();
        if (force) {
            File[] newCompileList = new File[compileList.length + files.length];
            System.arraycopy(compileList, 0, newCompileList, 0, compileList.length);
            int j = 0;
            for (int i = 0; i < files.length; i++)
                if (hasLegalSuffix(sfx, files[i])) {
                    if (srcDir == null)
                        newCompileList[compileList.length + (j++)] = new File(files[i]);
                    else
                        newCompileList[compileList.length + (j++)] =
                            fileUtils.resolveFile(srcDir, files[i]);
                }
            if (j == files.length)
            	compileList = newCompileList;
            else {
				compileList = new File[j];
				System.arraycopy(newCompileList, 0, compileList, 0, j);
            }
        } else {
            GlobPatternMapper m = new GlobPatternMapper();
            for (int i = 0; i < sfx.length; i++) {
                m.setFrom("*" + sfx[i]);
                m.setTo("*.class");
                SourceFileScanner sfs = new SourceFileScanner(this);
                File[] newFiles = sfs.restrictAsFiles(files, srcDir, destDir, m);
                if (newFiles.length > 0) {
                    File[] newCompileList = new File[compileList.length +
                        newFiles.length];
                    System.arraycopy(compileList, 0, newCompileList, 0,
                            compileList.length);
                    System.arraycopy(newFiles, 0, newCompileList,
                            compileList.length, newFiles.length);
                    compileList = newCompileList;
                }
            }
        }
    }
}
