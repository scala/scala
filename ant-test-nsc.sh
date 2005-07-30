#!/bin/sh

. ant-common.sh

# jar with fjbg, scala runtime
if ! addJar $fjbg_jar fjbg_jar;  then exit -1; fi
if ! addJar $tools_jar tools_jar;  then exit -1; fi
if ! addJar $scala_jar scala_jar; then echo "try: make jar target=LIBRARY" && exit -1; fi

# jars for `nsc' task (once its compiled)
if ! addJar $jars_dir/nsc4ant.jar "jars_dir containing nsc4ant";   then echo "try 'sh ant-build-nsc.sh build.nsc4'" && exit -1; fi
if ! addJar $jars_dir/nsc.jar "jars_dir containing nsc.jar";       then echo "try 'sh ant-build-nsc.sh'" && exit -1; fi

export CLASSPATH

# for debugging your classpath
#echo $CLASSPATH
if [ -f developer/${USER}/test-nsc-excludes.xml ]; then
  sed -e "s/userExcludes\ \"\"/userExcludes\ SYSTEM\ \"developer\/${USER}\/test-nsc-excludes.xml\"/" < test-nsc.xml > concrete-test-nsc.xml;
 else
  ln -s test-nsc.xml concrete-test-nsc.xml;
fi

ant -f concrete-test-nsc.xml $*
rm -f concrete-test-ncs.xml

