package scala.tools.scala4ant;

import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.taskdefs.Javac;

// obsolete

class AntTask extends Javac {
  override def execute():unit = {
    throw new BuildException("please use ScalacTask$class instead of AntTask$class");
  }

}
