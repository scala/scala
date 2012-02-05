#!/usr/bin/env bash
#
# Library to push and pull binary artifacts from a remote repository using CURL.


remote_urlbase="http://typesafe.artifactoryonline.com/typesafe/scala-sha-bootstrap/org/scala-lang/bootstrap"
libraryJar="$(pwd)/lib/scala-library.jar"
desired_ext=".desired.sha1"
push_jar="$(pwd)/tools/push.jar"
if [[ "$OSTYPE" == *Cygwin* || "$OSTYPE" == *cygwin* ]]; then push_jar="$(cygpath -m "$push_jar")"; fi
# Cache dir has .sbt in it to line up with SBT build.
cache_dir="${HOME}/.sbt/cache/scala"

# Checks whether or not curl is installed and issues a warning on failure.
checkCurl() {
 if ! which curl >/dev/null; then
    cat <<EOM
No means of downloading or uploading binary artifacts found.   

Please install curl for your OS.  e.g.
* sudo apt-get install curl
* brew install curl
EOM
  fi
}

# Executes the `curl` command to publish artifacts into a maven/ivy repository.
# Arugment 1 - The location to publish the file.
# Argument 2 - The file to publish.
# Argument 3 - The user to publish as.
# Argument 4 - The password for the user.
curlUpload() {
  checkCurl
  local remote_location=$1
  local data=$2
  local user=$3
  local password=$4
  local url="${remote_urlbase}/${remote_location}"
  java -jar $push_jar "$data" "$remote_location" "$user" "$password"
  if (( $? != 0 )); then
    echo "Error uploading $data to $url"
    echo "$url"
    exit 1
  fi
}

# Executes the `curl` command to download a file.
# Argument 1 - The location to store the file.
# Argument 2 - The URL to download.
curlDownload() {
  checkCurl
  local jar=$1
  local url=$2
  if [[ "$OSTYPE" == *Cygwin* || "$OSTYPE" == *cygwin* ]]; then
    jar=$(cygpath -m $1)
  fi
  http_code=$(curl --write-out '%{http_code}' --silent --fail --output "$jar" "$url")
  if (( $? != 0 )); then
    echo "Error downloading $jar: response code: $http_code"
    echo "$url"
    exit 1
  fi
}

# Pushes a local JAR file to the remote repository and updates the .desired.sha1 file.
# Argument 1 - The JAR file to update.
# Argument 2 - The root directory of the project.
# Argument 3 - The user to use when publishing artifacts.
# Argument 4 - The password to use when publishing artifacts.
pushJarFile() {
  local jar=$1
  local basedir=$2
  local user=$3
  local pw=$4
  local jar_dir=$(dirname $jar)
  local jar_name=${jar#$jar_dir/}
  pushd $jar_dir >/dev/null
  local jar_sha1=$(shasum -p $jar_name)
  local version=${jar_sha1% ?$jar_name}
  local remote_uri=${version}${jar#$basedir}
  echo "  Pushing to ${remote_urlbase}/${remote_uri} ..."
  echo "	$curl"
  curlUpload $remote_uri $jar_name $user $pw
  echo "  Making new sha1 file ...."
  echo "$jar_sha1" > "${jar_name}${desired_ext}"
  popd >/dev/null
  # TODO - Git remove jar and git add jar.desired.sha1
  # rm $jar
}

# Tests whether or not the .desired.sha1 hash matches a given file.
# Arugment 1 - The jar file to test validity.
# Returns: Empty string on failure, "OK" on success.
isJarFileValid() {
  local jar=$1
  if [[ ! -f $jar ]]; then
    echo ""
  else
    local jar_dir=$(dirname $jar)
    local jar_name=${jar#$jar_dir/}
    pushd $jar_dir >/dev/null
    local valid=$(shasum -p --check ${jar_name}${desired_ext} 2>/dev/null)
    echo "${valid#$jar_name: }"
    popd >/dev/null
  fi
}

# Pushes any jar file in the local repository for which the corresponding SHA1 hash is invalid or nonexistent.
# Argument 1 - The base directory of the project.
# Argument 2 - The user to use when pushing artifacts.
# Argument 3 - The password to use when pushing artifacts.
pushJarFiles() {
  local basedir=$1
  local user=$2
  local password=$3
  # TODO - ignore target/ and build/
  local jarFiles="$(find ${basedir}/lib -name "*.jar") $(find ${basedir}/test/files -name "*.jar")"
  local changed="no"
  for jar in $jarFiles; do
    local valid=$(isJarFileValid $jar)
    if [[ "$valid" != "OK" ]]; then
      echo "$jar has changed, pushing changes...."
      changed="yes"
      pushJarFile $jar $basedir $user $password
    fi
  done
  if test "$changed" == "no"; then
    echo "No jars have been changed."
  else
    echo "Binary changes have been pushed.  You may now submit the new *${desired_ext} files to git."
  fi
} 

# Pulls a single binary artifact from a remote repository.
# Argument 1 - The uri to the file that should be downloaded.
# Argument 2 - SHA of the file...
# Returns: Cache location.
pullJarFileToCache() {
  local uri=$1
  local sha=$2
  local cache_loc=$cache_dir/$uri
  local cdir=$(dirname $cache_loc)
  if [[ ! -d $cdir ]]; then
    mkdir -p $cdir
  fi
  # TODO - Check SHA of local cache is accurate.
  if [[ ! -f $cache_loc ]]; then
    curlDownload $cache_loc ${remote_urlbase}/${uri}
  fi
  echo "$cache_loc"
}

# Pulls a single binary artifact from a remote repository.
# Argument 1 - The jar file that needs to be downloaded.
# Argument 2 - The root directory of the project.
pullJarFile() {
  local jar=$1
  local basedir=$2
  local sha1=$(cat ${jar}${desired_ext})
  local jar_dir=$(dirname $jar)
  local jar_name=${jar#$jar_dir/}
  local version=${sha1% ?$jar_name}
  local remote_uri=${version}/${jar#$basedir/}
  echo "Resolving [${remote_uri}]"
  local cached_file=$(pullJarFileToCache $remote_uri $version)
  cp $cached_file $jar
}

# Pulls binary artifacts from the remote repository.
# Argument 1 - The directory to search for *.desired.sha1 files that need to be retrieved.
pullJarFiles() {
  local basedir=$1
  local desiredFiles="$(find ${basedir}/lib -name *${desired_ext}) $(find ${basedir}/test/files -name *${desired_ext}) $(find ${basedir}/tools -name *${desired_ext})"
  for sha in $desiredFiles; do
    jar=${sha%$desired_ext}
    local valid=$(isJarFileValid $jar)
    if [[ "$valid" != "OK" ]]; then
      pullJarFile $jar $basedir
    fi
  done
}


