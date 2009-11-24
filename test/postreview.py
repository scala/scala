#!/usr/bin/env python
import cookielib
import difflib
import getpass
import marshal
import mimetools
import ntpath
import os
import re
import socket
import stat
import subprocess
import sys
import tempfile
import urllib
import urllib2
from optparse import OptionParser
from tempfile import mkstemp
from urlparse import urljoin, urlparse

try:
    from hashlib import md5
except ImportError:
    # Support Python versions before 2.5.
    from md5 import md5

try:
    import json
except ImportError:
    import simplejson as json

# This specific import is necessary to handle the paths for
# cygwin enabled machines.
if (sys.platform.startswith('win')
    or sys.platform.startswith('cygwin')):
    import ntpath as cpath
else:
    import posixpath as cpath

###
# Default configuration -- user-settable variables follow.
###

# The following settings usually aren't needed, but if your Review
# Board crew has specific preferences and doesn't want to express
# them with command line switches, set them here and you're done.
# In particular, setting the REVIEWBOARD_URL variable will allow
# you to make it easy for people to submit reviews regardless of
# their SCM setup.
#
# Note that in order for this script to work with a reviewboard site
# that uses local paths to access a repository, the 'Mirror path'
# in the repository setup page must be set to the remote URL of the
# repository.

#
# Reviewboard URL.
#
# Set this if you wish to hard-code a default server to always use.
# It's generally recommended to set this using your SCM repository
# (for those that support it -- currently only SVN, Git, and Perforce).
#
# For example, on SVN:
#   $ svn propset reviewboard:url http://reviewboard.example.com .
#
# Or with Git:
#   $ git config reviewboard.url http://reviewboard.example.com
#
# On Perforce servers version 2008.1 and above:
#   $ p4 counter reviewboard.url http://reviewboard.example.com
#
# Older Perforce servers only allow numerical counters, so embedding
# the url in the counter name is also supported:
#   $ p4 counter reviewboard.url.http:\|\|reviewboard.example.com 1
#
# Note that slashes are not allowed in Perforce counter names, so replace them
# with pipe characters (they are a safe substitute as they are not used
# unencoded in URLs). You may need to escape them when issuing the p4 counter
# command as above.
#
# If this is not possible or desired, setting the value here will let
# you get started quickly.
#
# For all other repositories, a .reviewboardrc file present at the top of
# the checkout will also work. For example:
#
#   $ cat .reviewboardrc
#   REVIEWBOARD_URL = "http://reviewboard.example.com"
#
REVIEWBOARD_URL = None

# Default submission arguments.  These are all optional; run this
# script with --help for descriptions of each argument.
TARGET_GROUPS   = None
TARGET_PEOPLE   = None
SUBMIT_AS       = None
PUBLISH         = False
OPEN_BROWSER    = False

# Debugging.  For development...
DEBUG           = False

###
# End user-settable variables.
###


VERSION = "0.8"

user_config = None
tempfiles = []
options = None


class APIError(Exception):
    pass


class RepositoryInfo:
    """
    A representation of a source code repository.
    """
    def __init__(self, path=None, base_path=None, supports_changesets=False,
                 supports_parent_diffs=False):
        self.path = path
        self.base_path = base_path
        self.supports_changesets = supports_changesets
        self.supports_parent_diffs = supports_parent_diffs
        debug("repository info: %s" % self)

    def __str__(self):
        return "Path: %s, Base path: %s, Supports changesets: %s" % \
            (self.path, self.base_path, self.supports_changesets)

    def set_base_path(self, base_path):
        if not base_path.startswith('/'):
            base_path = '/' + base_path
        debug("changing repository info base_path from %s to %s" % \
              (self.base_path, base_path))
        self.base_path = base_path

    def find_server_repository_info(self, server):
        """
        Try to find the repository from the list of repositories on the server.
        For Subversion, this could be a repository with a different URL. For
        all other clients, this is a noop.
        """
        return self


class SvnRepositoryInfo(RepositoryInfo):
    """
    A representation of a SVN source code repository. This version knows how to
    find a matching repository on the server even if the URLs differ.
    """
    def __init__(self, path, base_path, uuid, supports_parent_diffs=False):
        RepositoryInfo.__init__(self, path, base_path,
                                supports_parent_diffs=supports_parent_diffs)
        self.uuid = uuid

    def find_server_repository_info(self, server):
        """
        The point of this function is to find a repository on the server that
        matches self, even if the paths aren't the same. (For example, if self
        uses an 'http' path, but the server uses a 'file' path for the same
        repository.) It does this by comparing repository UUIDs. If the
        repositories use the same path, you'll get back self, otherwise you'll
        get a different SvnRepositoryInfo object (with a different path).
        """
        repositories = server.get_repositories()

        for repository in repositories:
            if repository['tool'] != 'Subversion':
                continue

            info = self._get_repository_info(server, repository)

            if not info or self.uuid != info['uuid']:
                continue

            repos_base_path = info['url'][len(info['root_url']):]
            relpath = self._get_relative_path(self.base_path, repos_base_path)
            if relpath:
                return SvnRepositoryInfo(info['url'], relpath, self.uuid)

        # We didn't find a matching repository on the server. We'll just return
        # self and hope for the best.
        return self

    def _get_repository_info(self, server, repository):
        try:
            return server.get_repository_info(repository['id'])
        except APIError, e:
            # If the server couldn't fetch the repository info, it will return
            # code 210. Ignore those.
            # Other more serious errors should still be raised, though.
            rsp = e.args[0]
            if rsp['err']['code'] == 210:
                return None

            raise e

    def _get_relative_path(self, path, root):
        pathdirs = self._split_on_slash(path)
        rootdirs = self._split_on_slash(root)

        # root is empty, so anything relative to that is itself
        if len(rootdirs) == 0:
            return path

        # If one of the directories doesn't match, then path is not relative
        # to root.
        if rootdirs != pathdirs:
            return None

        # All the directories matched, so the relative path is whatever
        # directories are left over. The base_path can't be empty, though, so
        # if the paths are the same, return '/'
        if len(pathdirs) == len(rootdirs):
            return '/'
        else:
            return '/'.join(pathdirs[len(rootdirs):])

    def _split_on_slash(self, path):
        # Split on slashes, but ignore multiple slashes and throw away any
        # trailing slashes.
        split = re.split('/*', path)
        if split[-1] == '':
            split = split[0:-1]
        return split


class ReviewBoardHTTPPasswordMgr(urllib2.HTTPPasswordMgr):
    """
    Adds HTTP authentication support for URLs.

    Python 2.4's password manager has a bug in http authentication when the
    target server uses a non-standard port.  This works around that bug on
    Python 2.4 installs. This also allows post-review to prompt for passwords
    in a consistent way.

    See: http://bugs.python.org/issue974757
    """
    def __init__(self, reviewboard_url):
        self.passwd  = {}
        self.rb_url  = reviewboard_url
        self.rb_user = None
        self.rb_pass = None

    def find_user_password(self, realm, uri):
        if uri.startswith(self.rb_url):
            if self.rb_user is None or self.rb_pass is None:
                print "==> HTTP Authentication Required"
                print 'Enter username and password for "%s" at %s' % \
                    (realm, urlparse(uri)[1])
                self.rb_user = raw_input('Username: ')
                self.rb_pass = getpass.getpass('Password: ')

            return self.rb_user, self.rb_pass
        else:
            # If this is an auth request for some other domain (since HTTP
            # handlers are global), fall back to standard password management.
            return urllib2.HTTPPasswordMgr.find_user_password(self, realm, uri)


class ReviewBoardServer(object):
    """
    An instance of a Review Board server.
    """
    def __init__(self, url, info, cookie_file):
        self.url = url
        if self.url[-1] != '/':
            self.url += '/'
        self._info = info
        self._server_info = None
        self.cookie_file = cookie_file
        self.cookie_jar  = cookielib.MozillaCookieJar(self.cookie_file)

        # Set up the HTTP libraries to support all of the features we need.
        cookie_handler = urllib2.HTTPCookieProcessor(self.cookie_jar)
        password_mgr   = ReviewBoardHTTPPasswordMgr(self.url)
        auth_handler   = urllib2.HTTPBasicAuthHandler(password_mgr)

        opener = urllib2.build_opener(cookie_handler, auth_handler)
        opener.addheaders = [('User-agent', 'post-review/' + VERSION)]
        urllib2.install_opener(opener)

    def login(self, force=False):
        """
        Logs in to a Review Board server, prompting the user for login
        information if needed.
        """
        if not force and self.has_valid_cookie():
            return

        print "==> Review Board Login Required"
        print "Enter username and password for Review Board at %s" % self.url
        if options.username:
            username = options.username
        elif options.submit_as:
            username = options.submit_as
        else:
            username = raw_input('Username: ')

        if not options.password:
            password = getpass.getpass('Password: ')
        else:
            password = options.password

        debug('Logging in with username "%s"' % username)
        try:
            self.api_post('api/json/accounts/login/', {
                'username': username,
                'password': password,
            })
        except APIError, e:
            rsp, = e.args

            die("Unable to log in: %s (%s)" % (rsp["err"]["msg"],
                                               rsp["err"]["code"]))

        debug("Logged in.")

    def has_valid_cookie(self):
        """
        Load the user's cookie file and see if they have a valid
        'rbsessionid' cookie for the current Review Board server.  Returns
        true if so and false otherwise.
        """
        try:
            parsed_url = urlparse(self.url)
            host = parsed_url[1]
            path = parsed_url[2] or '/'

            # Cookie files don't store port numbers, unfortunately, so
            # get rid of the port number if it's present.
            host = host.split(":")[0]

            debug("Looking for '%s %s' cookie in %s" % \
                  (host, path, self.cookie_file))
            self.cookie_jar.load(self.cookie_file, ignore_expires=True)

            try:
                cookie = self.cookie_jar._cookies[host][path]['rbsessionid']

                if not cookie.is_expired():
                    debug("Loaded valid cookie -- no login required")
                    return True

                debug("Cookie file loaded, but cookie has expired")
            except KeyError:
                debug("Cookie file loaded, but no cookie for this server")
        except IOError, error:
            debug("Couldn't load cookie file: %s" % error)

        return False

    def new_review_request(self, changenum, submit_as=None):
        """
        Creates a review request on a Review Board server, updating an
        existing one if the changeset number already exists.

        If submit_as is provided, the specified user name will be recorded as
        the submitter of the review request (given that the logged in user has
        the appropriate permissions).
        """
        try:
            debug("Attempting to create review request for %s" % changenum)
            data = { 'repository_path': self.info.path }

            if changenum:
                data['changenum'] = changenum

            if submit_as:
                debug("Submitting the review request as %s" % submit_as)
                data['submit_as'] = submit_as

            rsp = self.api_post('api/json/reviewrequests/new/', data)
        except APIError, e:
            rsp, = e.args

            if not options.diff_only:
                if rsp['err']['code'] == 204: # Change number in use
                    debug("Review request already exists. Updating it...")
                    rsp = self.api_post(
                        'api/json/reviewrequests/%s/update_from_changenum/' %
                        rsp['review_request']['id'])
                else:
                    raise e

        debug("Review request created")
        return rsp['review_request']

    def set_review_request_field(self, review_request, field, value):
        """
        Sets a field in a review request to the specified value.
        """
        rid = review_request['id']

        debug("Attempting to set field '%s' to '%s' for review request '%s'" %
              (field, value, rid))

        self.api_post('api/json/reviewrequests/%s/draft/set/' % rid, {
            field: value,
        })

    def get_review_request(self, rid):
        """
        Returns the review request with the specified ID.
        """
        rsp = self.api_get('api/json/reviewrequests/%s/' % rid)
        return rsp['review_request']

    def get_repositories(self):
        """
        Returns the list of repositories on this server.
        """
        rsp = self.api_get('/api/json/repositories/')
        return rsp['repositories']

    def get_repository_info(self, rid):
        """
        Returns detailed information about a specific repository.
        """
        rsp = self.api_get('/api/json/repositories/%s/info/' % rid)
        return rsp['info']

    def save_draft(self, review_request):
        """
        Saves a draft of a review request.
        """
        self.api_post("api/json/reviewrequests/%s/draft/save/" %
                      review_request['id'])
        debug("Review request draft saved")

    def upload_diff(self, review_request, diff_content, parent_diff_content):
        """
        Uploads a diff to a Review Board server.
        """
        debug("Uploading diff, size: %d" % len(diff_content))

        if parent_diff_content:
            debug("Uploading parent diff, size: %d" % len(parent_diff_content))

        fields = {}
        files = {}

        if self.info.base_path:
            fields['basedir'] = self.info.base_path

        files['path'] = {
            'filename': 'diff',
            'content': diff_content
        }

        if parent_diff_content:
            files['parent_diff_path'] = {
                'filename': 'parent_diff',
                'content': parent_diff_content
            }

        self.api_post('api/json/reviewrequests/%s/diff/new/' %
                      review_request['id'], fields, files)

    def publish(self, review_request):
        """
        Publishes a review request.
        """
        debug("Publishing")
        self.api_post('api/json/reviewrequests/%s/publish/' %
                      review_request['id'])

    def _get_server_info(self):
        if not self._server_info:
            self._server_info = self._info.find_server_repository_info(self)

        return self._server_info

    info = property(_get_server_info)

    def process_json(self, data):
        """
        Loads in a JSON file and returns the data if successful. On failure,
        APIError is raised.
        """
        rsp = json.loads(data)

        if rsp['stat'] == 'fail':
            raise APIError, rsp

        return rsp

    def http_get(self, path):
        """
        Performs an HTTP GET on the specified path, storing any cookies that
        were set.
        """
        debug('HTTP GETting %s' % path)

        url = self._make_url(path)

        try:
            rsp = urllib2.urlopen(url).read()
            self.cookie_jar.save(self.cookie_file)
            return rsp
        except urllib2.HTTPError, e:
            print "Unable to access %s (%s). The host path may be invalid" % \
                (url, e.code)
            try:
                debug(e.read())
            except AttributeError:
                pass
            die()

    def _make_url(self, path):
        """Given a path on the server returns a full http:// style url"""
        app = urlparse(self.url)[2]
        if path[0] == '/':
            url = urljoin(self.url, app[:-1] + path)
        else:
            url = urljoin(self.url, app + path)

        if not url.startswith('http'):
            url = 'http://%s' % url
        return url

    def api_get(self, path):
        """
        Performs an API call using HTTP GET at the specified path.
        """
        return self.process_json(self.http_get(path))

    def http_post(self, path, fields, files=None):
        """
        Performs an HTTP POST on the specified path, storing any cookies that
        were set.
        """
        if fields:
            debug_fields = fields.copy()
        else:
            debug_fields = {}

        if 'password' in debug_fields:
            debug_fields["password"] = "**************"
        url = self._make_url(path)
        debug('HTTP POSTing to %s: %s' % (url, debug_fields))

        content_type, body = self._encode_multipart_formdata(fields, files)
        headers = {
            'Content-Type': content_type,
            'Content-Length': str(len(body))
        }

        try:
            r = urllib2.Request(url, body, headers)
            data = urllib2.urlopen(r).read()
            self.cookie_jar.save(self.cookie_file)
            return data
        except urllib2.URLError, e:
            try:
                debug(e.read())
            except AttributeError:
                pass

            die("Unable to access %s. The host path may be invalid\n%s" % \
                (url, e))
        except urllib2.HTTPError, e:
            die("Unable to access %s (%s). The host path may be invalid\n%s" % \
                (url, e.code, e.read()))

    def api_post(self, path, fields=None, files=None):
        """
        Performs an API call using HTTP POST at the specified path.
        """
        return self.process_json(self.http_post(path, fields, files))

    def _encode_multipart_formdata(self, fields, files):
        """
        Encodes data for use in an HTTP POST.
        """
        BOUNDARY = mimetools.choose_boundary()
        content = ""

        fields = fields or {}
        files = files or {}

        for key in fields:
            content += "--" + BOUNDARY + "\r\n"
            content += "Content-Disposition: form-data; name=\"%s\"\r\n" % key
            content += "\r\n"
            content += fields[key] + "\r\n"

        for key in files:
            filename = files[key]['filename']
            value = files[key]['content']
            content += "--" + BOUNDARY + "\r\n"
            content += "Content-Disposition: form-data; name=\"%s\"; " % key
            content += "filename=\"%s\"\r\n" % filename
            content += "\r\n"
            content += value + "\r\n"

        content += "--" + BOUNDARY + "--\r\n"
        content += "\r\n"

        content_type = "multipart/form-data; boundary=%s" % BOUNDARY

        return content_type, content


class SCMClient(object):
    """
    A base representation of an SCM tool for fetching repository information
    and generating diffs.
    """
    def get_repository_info(self):
        return None

    def scan_for_server(self, repository_info):
        """
        Scans the current directory on up to find a .reviewboard file
        containing the server path.
        """
        server_url = self._get_server_from_config(user_config, repository_info)
        if server_url:
            return server_url

        for path in walk_parents(os.getcwd()):
            filename = os.path.join(path, ".reviewboardrc")
            if os.path.exists(filename):
                config = load_config_file(filename)
                server_url = self._get_server_from_config(config,
                                                          repository_info)
                if server_url:
                    return server_url

        return None

    def diff(self, args):
        """
        Returns the generated diff and optional parent diff for this
        repository.

        The returned tuple is (diff_string, parent_diff_string)
        """
        return (None, None)

    def diff_between_revisions(self, revision_range, args, repository_info):
        """
        Returns the generated diff between revisions in the repository.
        """
        return None

    def _get_server_from_config(self, config, repository_info):
        if 'REVIEWBOARD_URL' in config:
            return config['REVIEWBOARD_URL']
        elif 'TREES' in config:
            trees = config['TREES']
            if not isinstance(trees, dict):
                die("Warning: 'TREES' in config file is not a dict!")

            if repository_info.path in trees and \
               'REVIEWBOARD_URL' in trees[repository_info.path]:
                return trees[repository_info.path]['REVIEWBOARD_URL']

        return None


class CVSClient(SCMClient):
    """
    A wrapper around the cvs tool that fetches repository
    information and generates compatible diffs.
    """
    def get_repository_info(self):
        if not check_install("cvs"):
            return None

        cvsroot_path = os.path.join("CVS", "Root")

        if not os.path.exists(cvsroot_path):
            return None

        fp = open(cvsroot_path, "r")
        repository_path = fp.read().strip()
        fp.close()

        i = repository_path.find("@")
        if i != -1:
            repository_path = repository_path[i + 1:]

        i = repository_path.find(":")
        if i != -1:
            host = repository_path[:i]
            try:
                canon = socket.getfqdn(host)
                repository_path = repository_path.replace('%s:' % host,
                                                          '%s:' % canon)
            except socket.error, msg:
                debug("failed to get fqdn for %s, msg=%s" % (host, msg))

        return RepositoryInfo(path=repository_path)

    def diff(self, files):
        """
        Performs a diff across all modified files in a CVS repository.

        CVS repositories do not support branches of branches in a way that
        makes parent diffs possible, so we never return a parent diff
        (the second value in the tuple).
        """
        return (self.do_diff(files), None)

    def diff_between_revisions(self, revision_range, args, repository_info):
        """
        Performs a diff between 2 revisions of a CVS repository.
        """
        revs = []

        for rev in revision_range.split(":"):
            revs += ["-r", rev]

        return self.do_diff(revs)

    def do_diff(self, params):
        """
        Performs the actual diff operation through cvs diff, handling
        fake errors generated by CVS.
        """
        # Diff returns "1" if differences were found.
        return execute(["cvs", "diff", "-uN"] + params,
                        extra_ignore_errors=(1,))


class ClearCaseClient(SCMClient):
    """
    A wrapper around the clearcase tool that fetches repository
    information and generates compatible diffs.
    This client assumes that cygwin is installed on windows.
    """
    ccroot_path = "/view/reviewboard.diffview/vobs/"
    viewinfo = ""
    viewtype = "snapshot"

    def get_filename_hash(self, fname):
        # Hash the filename string so its easy to find the file later on.
        return md5(fname).hexdigest()

    def get_repository_info(self):
        if not check_install('cleartool help'):
            return None

        # We must be running this from inside a view.
        # Otherwise it doesn't make sense.
        self.viewinfo = execute(["cleartool", "pwv", "-short"])
        if self.viewinfo.startswith('\*\* NONE'):
            return None

        # Returning the hardcoded clearcase root path to match the server
        #   respository path.
        # There is no reason to have a dynamic path unless you have
        #   multiple clearcase repositories. This should be implemented.
        return RepositoryInfo(path=self.ccroot_path,
                              base_path=self.ccroot_path,
                              supports_parent_diffs=False)

    def get_previous_version(self, files):
        file = []
        curdir = os.getcwd()

        # Cygwin case must transform a linux-like path to windows like path
        #   including drive letter.
        if 'cygdrive' in curdir:
            where = curdir.index('cygdrive') + 9
            drive_letter = curdir[where:where+1]
            curdir = drive_letter + ":\\" + curdir[where+2:len(curdir)]

        for key in files:
            # Sometimes there is a quote in the filename. It must be removed.
            key = key.replace('\'', '')
            elem_path = cpath.normpath(os.path.join(curdir, key))

            # Removing anything before the last /vobs
            #   because it may be repeated.
            elem_path_idx = elem_path.rfind("/vobs")
            if elem_path_idx != -1:
                elem_path = elem_path[elem_path_idx:len(elem_path)].strip("\"")

            # Call cleartool to get this version and the previous version
            #   of the element.
            curr_version, pre_version = execute(
                ["cleartool", "desc", "-pre", elem_path])
            curr_version = cpath.normpath(curr_version)
            pre_version = pre_version.split(':')[1].strip()

            # If a specific version was given, remove it from the path
            #   to avoid version duplication
            if "@@" in elem_path:
                elem_path = elem_path[:elem_path.rfind("@@")]
            file.append(elem_path + "@@" + pre_version)
            file.append(curr_version)

        # Determnine if the view type is snapshot or dynamic.
        if os.path.exists(file[0]):
            self.viewtype = "dynamic"

        return file

    def get_extended_namespace(self, files):
        """
        Parses the file path to get the extended namespace
        """
        versions = self.get_previous_version(files)

        evfiles = []
        hlist = []

        for vkey in versions:
            # Verify if it is a checkedout file.
            if "CHECKEDOUT" in vkey:
                # For checkedout files just add it to the file list
                #   since it cannot be accessed outside the view.
                splversions = vkey[:vkey.rfind("@@")]
                evfiles.append(splversions)
            else:
                # For checkedin files.
                ext_path = []
                ver = []
                fname = ""      # fname holds the file name without the version.
                (bpath, fpath) = cpath.splitdrive(vkey)
                if bpath :
                    # Windows.
                    # The version (if specified like file.c@@/main/1)
                    #   should be kept as a single string
                    #   so split the path and concat the file name
                    #   and version in the last position of the list.
                    ver = fpath.split("@@")
                    splversions = fpath[:vkey.rfind("@@")].split("\\")
                    fname = splversions.pop()
                    splversions.append(fname + ver[1])
                else :
                    # Linux.
                    bpath = vkey[:vkey.rfind("vobs")+4]
                    fpath = vkey[vkey.rfind("vobs")+5:]
                    ver = fpath.split("@@")
                    splversions =  ver[0][:vkey.rfind("@@")].split("/")
                    fname = splversions.pop()
                    splversions.append(fname + ver[1])

                filename = splversions.pop()
                bpath = cpath.normpath(bpath + "/")
                elem_path = bpath

                for key in splversions:
                    # For each element (directory) in the path,
                    #   get its version from clearcase.
                    elem_path = cpath.join(elem_path, key)

                    # This is the version to be appended to the extended
                    #   path list.
                    this_version = execute(
                        ["cleartool", "desc", "-fmt", "%Vn",
                        cpath.normpath(elem_path)])
                    if this_version:
                        ext_path.append(key + "/@@" + this_version + "/")
                    else:
                        ext_path.append(key + "/")

                # This must be done in case we haven't specified
                #   the version on the command line.
                ext_path.append(cpath.normpath(fname + "/@@" +
                    vkey[vkey.rfind("@@")+2:len(vkey)]))
                epstr = cpath.join(bpath, cpath.normpath(''.join(ext_path)))
                evfiles.append(epstr)

                """
                In windows, there is a problem with long names(> 254).
                In this case, we hash the string and copy the unextended
                  filename to a temp file whose name is the hash.
                This way we can get the file later on for diff.
                The same problem applies to snapshot views where the
                  extended name isn't available.
                The previous file must be copied from the CC server
                  to a local dir.
                """
                if cpath.exists(epstr) :
                    pass
                else:
                    if len(epstr) > 254 or self.viewtype == "snapshot":
                        name = self.get_filename_hash(epstr)
                        # Check if this hash is already in the list
                        try:
                            i = hlist.index(name)
                            die("ERROR: duplicate value %s : %s" %
                                (name, epstr))
                        except ValueError:
                            hlist.append(name)

                        normkey = cpath.normpath(vkey)
                        td = tempfile.gettempdir()
                        # Cygwin case must transform a linux-like path to
                        # windows like path including drive letter
                        if 'cygdrive' in td:
                            where = td.index('cygdrive') + 9
                            drive_letter = td[where:where+1] + ":"
                            td = cpath.join(drive_letter, td[where+1:])
                        tf = cpath.normpath(cpath.join(td, name))
                        if cpath.exists(tf):
                            debug("WARNING: FILE EXISTS")
                            os.unlink(tf)
                        execute(["cleartool", "get", "-to", tf, normkey])
                    else:
                        die("ERROR: FILE NOT FOUND : %s" % epstr)

        return evfiles

    def get_files_from_label(self, label):
        voblist=[]
        # Get the list of vobs for the current view
        allvoblist = execute(["cleartool", "lsvob", "-short"]).split()
        # For each vob, find if the label is present
        for vob in allvoblist:
            try:
                execute(["cleartool", "describe", "-local",
                    "lbtype:%s@%s" % (label, vob)]).split()
                voblist.append(vob)
            except:
                pass

        filelist=[]
        # For each vob containing the label, get the file list
        for vob in voblist:
            try:
                res = execute(["cleartool", "find", vob, "-all", "-version",
                    "lbtype(%s)" % label, "-print"])
                filelist.extend(res.split())
            except :
                pass

        # Return only the unique itens
        return set(filelist)

    def diff(self, files):
        """
        Performs a diff of the specified file and its previous version.
        """
        # We must be running this from inside a view.
        # Otherwise it doesn't make sense.
        return self.do_diff(self.get_extended_namespace(files))

    def diff_label(self, label):
        """
        Get the files that are attached to a label and diff them
        TODO
        """
        return self.diff(self.get_files_from_label(label))

    def diff_between_revisions(self, revision_range, args, repository_info):
        """
        Performs a diff between 2 revisions of a CC repository.
        """
        rev_str = ''

        for rev in revision_range.split(":"):
            rev_str += "-r %s " % rev

        return self.do_diff(rev_str)

    def do_diff(self, params):
        # Diff returns "1" if differences were found.
        # Add the view name and view type to the description
        if options.description:
            options.description = ("VIEW: " + self.viewinfo +
                "VIEWTYPE: " + self.viewtype + "\n" + options.description)
        else:
            options.description = (self.viewinfo +
                "VIEWTYPE: " + self.viewtype + "\n")

        o = []
        Feol = False
        while len(params) > 0:
            # Read both original and modified files.
            onam = params.pop(0)
            mnam = params.pop(0)
            file_data = []
            do_rem = False
            # If the filename length is greater than 254 char for windows,
            #   we copied the file to a temp file
            #   because the open will not work for path greater than 254.
            # This is valid for the original and
            #   modified files if the name size is > 254.
            for filenam in (onam, mnam) :
                if cpath.exists(filenam) and self.viewtype == "dynamic":
                    do_rem = False
                    fn = filenam
                elif len(filenam) > 254 or self.viewtype == "snapshot":
                    fn = self.get_filename_hash(filenam)
                    fn = cpath.join(tempfile.gettempdir(), fn)
                    do_rem = True
                fd = open(cpath.normpath(fn))
                fdata = fd.readlines()
                fd.close()
                file_data.append(fdata)
                # If the file was temp, it should be removed.
                if do_rem:
                    os.remove(filenam)

            modi = file_data.pop()
            orig = file_data.pop()

            # For snapshot views, the local directories must be removed because
            #   they will break the diff on the server. Just replacing
            #   everything before the view name (including the view name) for
            #   vobs do the work.
            if (self.viewtype == "snapshot"
                and (sys.platform.startswith('win')
                  or sys.platform.startswith('cygwin'))):
                    vinfo = self.viewinfo.rstrip("\r\n")
                    mnam = "c:\\\\vobs" + mnam[mnam.rfind(vinfo) + len(vinfo):]
                    onam = "c:\\\\vobs" + onam[onam.rfind(vinfo) + len(vinfo):]
            # Call the diff lib to generate a diff.
            # The dates are bogus, since they don't natter anyway.
            # The only thing is that two spaces are needed to the server
            #   so it can identify the heades correctly.
            diff = difflib.unified_diff(orig, modi, onam, mnam,
               '  2002-02-21 23:30:39.942229878 -0800',
               '  2002-02-21 23:30:50.442260588 -0800', lineterm=' \n')
            # Transform the generator output into a string output
            #   Use a comprehension instead of a generator,
            #   so 2.3.x doesn't fail to interpret.
            diffstr = ''.join([str(l) for l in diff])
            # Workaround for the difflib no new line at end of file
            #   problem.
            if not diffstr.endswith('\n'):
                diffstr = diffstr + ("\n\\ No newline at end of file\n")
            o.append(diffstr)

        ostr = ''.join(o)
        return (ostr, None) # diff, parent_diff (not supported)


class SVNClient(SCMClient):
    """
    A wrapper around the svn Subversion tool that fetches repository
    information and generates compatible diffs.
    """
    def get_repository_info(self):
        if not check_install('svn help'):
            return None

        # Get the SVN repository path (either via a working copy or
        # a supplied URI)
        svn_info_params = ["svn", "info"]
        if options.repository_url:
            svn_info_params.append(options.repository_url)
        data = execute(svn_info_params,
                       ignore_errors=True)
        m = re.search(r'^Repository Root: (.+)$', data, re.M)
        if not m:
            return None

        path = m.group(1)

        m = re.search(r'^URL: (.+)$', data, re.M)
        if not m:
            return None

        base_path = m.group(1)[len(path):] or "/"

        m = re.search(r'^Repository UUID: (.+)$', data, re.M)
        if not m:
            return None

        return SvnRepositoryInfo(path, base_path, m.group(1))

    def scan_for_server(self, repository_info):
        # Scan first for dot files, since it's faster and will cover the
        # user's $HOME/.reviewboardrc
        server_url = super(SVNClient, self).scan_for_server(repository_info)
        if server_url:
            return server_url

        return self.scan_for_server_property(repository_info)

    def scan_for_server_property(self, repository_info):
        def get_url_prop(path):
            url = execute(["svn", "propget", "reviewboard:url", path]).strip()
            return url or None

        for path in walk_parents(os.getcwd()):
            if not os.path.exists(os.path.join(path, ".svn")):
                break

            prop = get_url_prop(path)
            if prop:
                return prop

        return get_url_prop(repository_info.path)

    def diff(self, files):
        """
        Performs a diff across all modified files in a Subversion repository.

        SVN repositories do not support branches of branches in a way that
        makes parent diffs possible, so we never return a parent diff
        (the second value in the tuple).
        """
        return (self.do_diff(["svn", "diff", "--diff-cmd=diff"] + files),
                None)

    def diff_between_revisions(self, revision_range, args, repository_info):
        """
        Performs a diff between 2 revisions of a Subversion repository.
        """
        if options.repository_url:
            revisions = revision_range.split(':')
            if len(revisions) < 1:
                return None
            elif len(revisions) == 1:
                revisions.append('HEAD')

            # if a new path was supplied at the command line, set it
            if len(args):
                repository_info.set_base_path(args[0])

            url = repository_info.path + repository_info.base_path

            old_url = url + '@' + revisions[0]
            new_url = url + '@' + revisions[1]

            return self.do_diff(["svn", "diff", "--diff-cmd=diff", old_url,
                                 new_url],
                                repository_info)
        # Otherwise, perform the revision range diff using a working copy
        else:
            return self.do_diff(["svn", "diff", "--diff-cmd=diff", "-r",
                                 revision_range],
                                repository_info)

    def do_diff(self, cmd, repository_info=None):
        """
        Performs the actual diff operation, handling renames and converting
        paths to absolute.
        """
        diff = execute(cmd, split_lines=True)
        diff = self.handle_renames(diff)
        diff = self.convert_to_absolute_paths(diff, repository_info)

        return ''.join(diff)

    def handle_renames(self, diff_content):
        """
        The output of svn diff is incorrect when the file in question came
        into being via svn mv/cp. Although the patch for these files are
        relative to its parent, the diff header doesn't reflect this.
        This function fixes the relevant section headers of the patch to
        portray this relationship.
        """

        # svn diff against a repository URL on two revisions appears to
        # handle moved files properly, so only adjust the diff file names
        # if they were created using a working copy.
        if options.repository_url:
            return diff_content

        result = []

        from_line = ""
        for line in diff_content:
            if line.startswith('--- '):
                from_line = line
                continue

            # This is where we decide how mangle the previous '--- '
            if line.startswith('+++ '):
                to_file, _ = self.parse_filename_header(line[4:])
                info       = self.svn_info(to_file)
                if info.has_key("Copied From URL"):
                    url       = info["Copied From URL"]
                    root      = info["Repository Root"]
                    from_file = urllib.unquote(url[len(root):])
                    result.append(from_line.replace(to_file, from_file))
                else:
                    result.append(from_line) #as is, no copy performed

            # We only mangle '---' lines. All others get added straight to
            # the output.
            result.append(line)

        return result


    def convert_to_absolute_paths(self, diff_content, repository_info):
        """
        Converts relative paths in a diff output to absolute paths.
        This handles paths that have been svn switched to other parts of the
        repository.
        """

        result = []

        for line in diff_content:
            front = None
            if line.startswith('+++ ') or line.startswith('--- ') or line.startswith('Index: '):
                front, line = line.split(" ", 1)

            if front:
                if line.startswith('/'): #already absolute
                    line = front + " " + line
                else:
                    # filename and rest of line (usually the revision
                    # component)
                    file, rest = self.parse_filename_header(line)

                    # If working with a diff generated outside of a working
                    # copy, then file paths are already absolute, so just
                    # add initial slash.
                    if options.repository_url:
                        path = urllib.unquote(
                            "%s/%s" % (repository_info.base_path, file))
                    else:
                        info = self.svn_info(file)
                        url  = info["URL"]
                        root = info["Repository Root"]
                        path = urllib.unquote(url[len(root):])

                    line = front + " " + path + rest

            result.append(line)

        return result

    def svn_info(self, path):
        """Return a dict which is the result of 'svn info' at a given path."""
        svninfo = {}
        for info in execute(["svn", "info", path],
                            split_lines=True):
            parts = info.strip().split(": ", 1)
            if len(parts) == 2:
                key, value = parts
                svninfo[key] = value

        return svninfo

    # Adapted from server code parser.py
    def parse_filename_header(self, s):
        parts = None
        if "\t" in s:
            # There's a \t separating the filename and info. This is the
            # best case scenario, since it allows for filenames with spaces
            # without much work.
            parts = s.split("\t")

        # There's spaces being used to separate the filename and info.
        # This is technically wrong, so all we can do is assume that
        # 1) the filename won't have multiple consecutive spaces, and
        # 2) there's at least 2 spaces separating the filename and info.
        if "  " in s:
            parts = re.split(r"  +", s)

        if parts:
            parts[1] = '\t' + parts[1]
            return parts

        # strip off ending newline, and return it as the second component
        return [s.split('\n')[0], '\n']


class PerforceClient(SCMClient):
    """
    A wrapper around the p4 Perforce tool that fetches repository information
    and generates compatible diffs.
    """
    def get_repository_info(self):
        if not check_install('p4 help'):
            return None

        data = execute(["p4", "info"], ignore_errors=True)

        m = re.search(r'^Server address: (.+)$', data, re.M)
        if not m:
            return None

        repository_path = m.group(1).strip()

        try:
            hostname, port = repository_path.split(":")
            info = socket.gethostbyaddr(hostname)
            repository_path = "%s:%s" % (info[0], port)
        except (socket.gaierror, socket.herror):
            pass

        return RepositoryInfo(path=repository_path, supports_changesets=True)

    def scan_for_server(self, repository_info):
        # Scan first for dot files, since it's faster and will cover the
        # user's $HOME/.reviewboardrc
        server_url = \
            super(PerforceClient, self).scan_for_server(repository_info)

        if server_url:
            return server_url

        return self.scan_for_server_counter(repository_info)

    def scan_for_server_counter(self, repository_info):
        """
        Checks the Perforce counters to see if the Review Board server's url
        is specified. Since Perforce only started supporting non-numeric
        counter values in server version 2008.1, we support both a normal
        counter 'reviewboard.url' with a string value and embedding the url in
        a counter name like 'reviewboard.url.http:||reviewboard.example.com'.
        Note that forward slashes aren't allowed in counter names, so
        pipe ('|') characters should be used. These should be safe because they
        should not be used unencoded in urls.
        """

        counters_text = execute(["p4", "counters"])

        # Try for a "reviewboard.url" counter first.
        m = re.search(r'^reviewboard.url = (\S+)', counters_text, re.M)

        if m:
            return m.group(1)

        # Next try for a counter of the form:
        # reviewboard_url.http:||reviewboard.example.com
        m2 = re.search(r'^reviewboard.url\.(\S+)', counters_text, re.M)

        if m2:
            return m2.group(1).replace('|', '/')

        return None

    def get_changenum(self, args):
        if len(args) == 1:
            try:
                return str(int(args[0]))
            except ValueError:
                pass
        return None

    def diff(self, args):
        """
        Goes through the hard work of generating a diff on Perforce in order
        to take into account adds/deletes and to provide the necessary
        revision information.
        """
        # set the P4 enviroment:
        if options.p4_client:
           os.environ['P4CLIENT'] = options.p4_client

        if options.p4_port:
           os.environ['P4PORT'] = options.p4_port

        changenum = self.get_changenum(args)
        if changenum is None:
            return self._path_diff(args)
        else:
            return self._changenum_diff(changenum)


    def _path_diff(self, args):
        """
        Process a path-style diff.  See _changenum_diff for the alternate
        version that handles specific change numbers.

        Multiple paths may be specified in `args`.  The path styles supported
        are:

        //path/to/file
        Upload file as a "new" file.

        //path/to/dir/...
        Upload all files as "new" files.

        //path/to/file[@#]rev
        Upload file from that rev as a "new" file.

        //path/to/file[@#]rev,[@#]rev
        Upload a diff between revs.

        //path/to/dir/...[@#]rev,[@#]rev
        Upload a diff of all files between revs in that directory.
        """
        r_revision_range = re.compile(r'^(?P<path>//[^@#]+)' +
                                      r'(?P<revision1>[#@][^,]+)?' +
                                      r'(?P<revision2>,[#@][^,]+)?$')

        empty_filename = make_tempfile()
        tmp_diff_from_filename = make_tempfile()
        tmp_diff_to_filename = make_tempfile()

        diff_lines = []

        for path in args:
            m = r_revision_range.match(path)

            if not m:
                die('Path %r does not match a valid Perforce path.' % (path,))
            revision1 = m.group('revision1')
            revision2 = m.group('revision2')
            first_rev_path = m.group('path')

            if revision1:
                first_rev_path += revision1
            records = self._run_p4(['files', first_rev_path])

            # Make a map for convenience.
            files = {}

            # Records are:
            # 'rev': '1'
            # 'func': '...'
            # 'time': '1214418871'
            # 'action': 'edit'
            # 'type': 'ktext'
            # 'depotFile': '...'
            # 'change': '123456'
            for record in records:
                if record['action'] != 'delete':
                    if revision2:
                        files[record['depotFile']] = [record, None]
                    else:
                        files[record['depotFile']] = [None, record]

            if revision2:
                # [1:] to skip the comma.
                second_rev_path = m.group('path') + revision2[1:]
                records = self._run_p4(['files', second_rev_path])
                for record in records:
                    if record['action'] != 'delete':
                        try:
                            m = files[record['depotFile']]
                            m[1] = record
                        except KeyError:
                            files[record['depotFile']] = [None, record]

            old_file = new_file = empty_filename
            changetype_short = None

            for depot_path, (first_record, second_record) in files.items():
                old_file = new_file = empty_filename
                if first_record is None:
                    self._write_file(depot_path + '#' + second_record['rev'],
                                     tmp_diff_to_filename)
                    new_file = tmp_diff_to_filename
                    changetype_short = 'A'
                    base_revision = 0
                elif second_record is None:
                    self._write_file(depot_path + '#' + first_record['rev'],
                                     tmp_diff_from_filename)
                    old_file = tmp_diff_from_filename
                    changetype_short = 'D'
                    base_revision = int(first_record['rev'])
                else:
                    self._write_file(depot_path + '#' + first_record['rev'],
                                     tmp_diff_from_filename)
                    self._write_file(depot_path + '#' + second_record['rev'],
                                     tmp_diff_to_filename)
                    new_file = tmp_diff_to_filename
                    old_file = tmp_diff_from_filename
                    changetype_short = 'M'
                    base_revision = int(first_record['rev'])

                dl = self._do_diff(old_file, new_file, depot_path,
                                   base_revision, changetype_short,
                                   ignore_unmodified=True)
                diff_lines += dl

        os.unlink(empty_filename)
        os.unlink(tmp_diff_from_filename)
        os.unlink(tmp_diff_to_filename)
        return (''.join(diff_lines), None)

    def _run_p4(self, command):
        """Execute a perforce command using the python marshal API.

        - command: A list of strings of the command to execute.

        The return type depends on the command being run.
        """
        command = ['p4', '-G'] + command
        p = subprocess.Popen(command, stdout=subprocess.PIPE)
        result = []
        has_error = False

        while 1:
            try:
                data = marshal.load(p.stdout)
            except EOFError:
                break
            else:
                result.append(data)
                if data.get('code', None) == 'error':
                    has_error = True

        rc = p.wait()

        if rc or has_error:
            for record in result:
                if 'data' in record:
                    print record['data']
            die('Failed to execute command: %s\n' % (command,))

        return result

    def _changenum_diff(self, changenum):
        """
        Process a diff for a particular change number.  This handles both
        pending and submitted changelists.

        See _path_diff for the alternate version that does diffs of depot
        paths.
        """
        # TODO: It might be a good idea to enhance PerforceDiffParser to
        # understand that newFile could include a revision tag for post-submit
        # reviewing.
        cl_is_pending = False

        debug("Generating diff for changenum %s" % changenum)

        description = execute(["p4", "describe", "-s", changenum],
                              split_lines=True)

        if '*pending*' in description[0]:
            cl_is_pending = True

        # Get the file list
        for line_num, line in enumerate(description):
            if 'Affected files ...' in line:
                break
        else:
            # Got to the end of all the description lines and didn't find
            # what we were looking for.
            die("Couldn't find any affected files for this change.")

        description = description[line_num+2:]

        diff_lines = []

        empty_filename = make_tempfile()
        tmp_diff_from_filename = make_tempfile()
        tmp_diff_to_filename = make_tempfile()

        for line in description:
            line = line.strip()
            if not line:
                continue

            m = re.search(r'\.\.\. ([^#]+)#(\d+) (add|edit|delete|integrate|branch)', line)
            if not m:
                die("Unsupported line from p4 opened: %s" % line)

            depot_path = m.group(1)
            base_revision = int(m.group(2))
            if not cl_is_pending:
                # If the changelist is pending our base revision is the one that's
                # currently in the depot. If we're not pending the base revision is
                # actually the revision prior to this one
                base_revision -= 1

            changetype = m.group(3)

            debug('Processing %s of %s' % (changetype, depot_path))

            old_file = new_file = empty_filename
            old_depot_path = new_depot_path = None
            changetype_short = None

            if changetype == 'edit' or changetype == 'integrate':
                # A big assumption
                new_revision = base_revision + 1

                # We have an old file, get p4 to take this old version from the
                # depot and put it into a plain old temp file for us
                old_depot_path = "%s#%s" % (depot_path, base_revision)
                self._write_file(old_depot_path, tmp_diff_from_filename)
                old_file = tmp_diff_from_filename

                # Also print out the new file into a tmpfile
                if cl_is_pending:
                    new_file = self._depot_to_local(depot_path)
                else:
                    new_depot_path = "%s#%s" %(depot_path, new_revision)
                    self._write_file(new_depot_path, tmp_diff_to_filename)
                    new_file = tmp_diff_to_filename

                changetype_short = "M"

            elif changetype == 'add' or changetype == 'branch':
                # We have a new file, get p4 to put this new file into a pretty
                # temp file for us. No old file to worry about here.
                if cl_is_pending:
                    new_file = self._depot_to_local(depot_path)
                else:
                    self._write_file(depot_path, tmp_diff_to_filename)
                    new_file = tmp_diff_to_filename
                changetype_short = "A"

            elif changetype == 'delete':
                # We've deleted a file, get p4 to put the deleted file into  a temp
                # file for us. The new file remains the empty file.
                old_depot_path = "%s#%s" % (depot_path, base_revision)
                self._write_file(old_depot_path, tmp_diff_from_filename)
                old_file = tmp_diff_from_filename
                changetype_short = "D"
            else:
                die("Unknown change type '%s' for %s" % (changetype, depot_path))

            dl = self._do_diff(old_file, new_file, depot_path, base_revision, changetype_short)
            diff_lines += dl

        os.unlink(empty_filename)
        os.unlink(tmp_diff_from_filename)
        os.unlink(tmp_diff_to_filename)
        return (''.join(diff_lines), None)

    def _do_diff(self, old_file, new_file, depot_path, base_revision,
                 changetype_short, ignore_unmodified=False):
        """
        Do the work of producing a diff for Perforce.

        old_file - The absolute path to the "old" file.
        new_file - The absolute path to the "new" file.
        depot_path - The depot path in Perforce for this file.
        base_revision - The base perforce revision number of the old file as
            an integer.
        changetype_short - The change type as a single character string.
        ignore_unmodified - If True, will return an empty list if the file
            is not changed.

        Returns a list of strings of diff lines.
        """
        if hasattr(os, 'uname') and os.uname()[0] == 'SunOS':
            diff_cmd = ["gdiff", "-urNp", old_file, new_file]
        else:
            diff_cmd = ["diff", "-urNp", old_file, new_file]
        # Diff returns "1" if differences were found.
        dl = execute(diff_cmd, extra_ignore_errors=(1,2),
                     translate_newlines=False)

        # If the input file has ^M characters at end of line, lets ignore them.
        dl = dl.replace('\r\r\n', '\r\n')
        dl = dl.splitlines(True)

        cwd = os.getcwd()
        if depot_path.startswith(cwd):
            local_path = depot_path[len(cwd) + 1:]
        else:
            local_path = depot_path

        # Special handling for the output of the diff tool on binary files:
        #     diff outputs "Files a and b differ"
        # and the code below expects the output to start with
        #     "Binary files "
        if len(dl) == 1 and \
           dl[0] == ('Files %s and %s differ'% (old_file, new_file)):
            dl = ['Binary files %s and %s differ'% (old_file, new_file)]

        if dl == [] or dl[0].startswith("Binary files "):
            if dl == []:
                if ignore_unmodified:
                    return []
                else:
                    print "Warning: %s in your changeset is unmodified" % \
                        local_path

            dl.insert(0, "==== %s#%s ==%s== %s ====\n" % \
                (depot_path, base_revision, changetype_short, local_path))
            dl.append('\n')
        else:
            m = re.search(r'(\d\d\d\d-\d\d-\d\d \d\d:\d\d:\d\d)', dl[1])
            if m:
                timestamp = m.group(1)
            else:
                # Thu Sep  3 11:24:48 2007
                m = re.search(r'(\w+)\s+(\w+)\s+(\d+)\s+(\d\d:\d\d:\d\d)\s+(\d\d\d\d)', dl[1])
                if not m:
                    die("Unable to parse diff header: %s" % dl[1])

                month_map = {
                    "Jan": "01",
                    "Feb": "02",
                    "Mar": "03",
                    "Apr": "04",
                    "May": "05",
                    "Jun": "06",
                    "Jul": "07",
                    "Aug": "08",
                    "Sep": "09",
                    "Oct": "10",
                    "Nov": "11",
                    "Dec": "12",
                }
                month = month_map[m.group(2)]
                day = m.group(3)
                timestamp = m.group(4)
                year = m.group(5)

                timestamp = "%s-%s-%s %s" % (year, month, day, timestamp)

            dl[0] = "--- %s\t%s#%s\n" % (local_path, depot_path, base_revision)
            dl[1] = "+++ %s\t%s\n" % (local_path, timestamp)

        return dl

    def _write_file(self, depot_path, tmpfile):
        """
        Grabs a file from Perforce and writes it to a temp file. p4 print sets
        the file readonly and that causes a later call to unlink fail. So we
        make the file read/write.
        """
        debug('Writing "%s" to "%s"' % (depot_path, tmpfile))
        execute(["p4", "print", "-o", tmpfile, "-q", depot_path])
        os.chmod(tmpfile, stat.S_IREAD | stat.S_IWRITE)

    def _depot_to_local(self, depot_path):
        """
        Given a path in the depot return the path on the local filesystem to
        the same file.  If there are multiple results, take only the last
        result from the where command.
        """
        where_output = self._run_p4(['where', depot_path])
        return where_output[-1]['path']


class MercurialClient(SCMClient):
    """
    A wrapper around the hg Mercurial tool that fetches repository
    information and generates compatible diffs.
    """
    def get_repository_info(self):
        if not check_install('hg --help'):
            return None

        data = execute(["hg", "root"], ignore_errors=True)
        if data.startswith('abort:'):
            # hg aborted => no mercurial repository here.
            return None

        # Elsewhere, hg root output give us the repository path.

        # We save data here to use it as a fallback. See below
        local_data = data.strip()

        svn = execute(["hg", "svn", "info", ], ignore_errors=True)

        if (not svn.startswith('abort:') and
            not svn.startswith("hg: unknown command")):
            self.type = 'svn'
            m = re.search(r'^Repository Root: (.+)$', svn, re.M)

            if not m:
                return None

            path = m.group(1)
            m2 = re.match(r'^(svn\+ssh|http|https)://([-a-zA-Z0-9.]*@)(.*)$',
                          path)
            if m2:
                path = '%s://%s' % (m2.group(1), m2.group(3))

            m = re.search(r'^URL: (.+)$', svn, re.M)

            if not m:
                return None

            base_path = m.group(1)[len(path):] or "/"
            return RepositoryInfo(path=path,
                                  base_path=base_path,
                                  supports_parent_diffs=True)

        self.type = 'hg'

        # We are going to search .hg/hgrc for the default path.
        file_name = os.path.join(local_data,'.hg', 'hgrc')

        if not os.path.exists(file_name):
            return RepositoryInfo(path=local_data, base_path='/',
                                  supports_parent_diffs=True)

        f = open(file_name)
        data = f.read()
        f.close()

        m = re.search(r'^default\s+=\s+(.+)$', data, re.M)

        if not m:
            # Return the local path, if no default value is found.
            return RepositoryInfo(path=local_data, base_path='/',
                                  supports_parent_diffs=True)

        path = m.group(1).strip()

        return RepositoryInfo(path=path, base_path='',
                              supports_parent_diffs=True)

    def diff(self, files):
        """
        Performs a diff across all modified files in a Mercurial repository.
        """
        # We don't support parent diffs with Mercurial yet, so we always
        # return None for the parent diff.
        if self.type == 'svn':
            parent = execute(['hg', 'parent', '--svn', '--template',
                              '{node}\n']).strip()

            if options.parent_branch:
                parent = options.parent_branch

            if options.guess_summary and not options.summary:
                options.summary = execute(['hg', 'log', '-r.', '--template',
                                            r'{desc|firstline}\n'])

            if options.guess_description and not options.description:
                numrevs = len(execute(['hg', 'log', '-r.:%s' % parent,
                                       '--follow', '--template',
                                       r'{rev}\n']).strip().split('\n'))
                options.description = execute(['hg', 'log', '-r.:%s' % parent,
                                               '--follow', '--template',
                                               r'{desc}\n\n', '--limit',
                                               str(numrevs-1)]).strip()

            return (execute(["hg", "diff", "--svn", '-r%s:.' % parent]), None)

        return (execute(["hg", "diff"] + files), None)

    def diff_between_revisions(self, revision_range, args, repository_info):
        """
        Performs a diff between 2 revisions of a Mercurial repository.
        """
        if self.type != 'hg':
            raise NotImplementedError

        r1, r2 = revision_range.split(':')
        return execute(["hg", "diff", "-r", r1, "-r", r2])


class GitClient(SCMClient):
    """
    A wrapper around git that fetches repository information and generates
    compatible diffs. This will attempt to generate a diff suitable for the
    remote repository, whether git, SVN or Perforce.
    """
    def get_repository_info(self):
        if not check_install('git --help'):
            return None

        git_dir = execute(["git", "rev-parse", "--git-dir"],
                          ignore_errors=True).strip()

        if git_dir.startswith("fatal:") or not os.path.isdir(git_dir):
            return None

        # post-review in directories other than the top level of
        # of a work-tree would result in broken diffs on the server
        os.chdir(os.path.dirname(os.path.abspath(git_dir)))

        # We know we have something we can work with. Let's find out
        # what it is. We'll try SVN first.
        data = execute(["git", "svn", "info"], ignore_errors=True)

        m = re.search(r'^Repository Root: (.+)$', data, re.M)
        if m:
            path = m.group(1)
            m = re.search(r'^URL: (.+)$', data, re.M)

            if m:
                base_path = m.group(1)[len(path):] or "/"
                m = re.search(r'^Repository UUID: (.+)$', data, re.M)

                if m:
                    uuid = m.group(1)
                    self.type = "svn"

                    return SvnRepositoryInfo(path=path, base_path=base_path,
                                             uuid=uuid,
                                             supports_parent_diffs=True)
        else:
            # Versions of git-svn before 1.5.4 don't (appear to) support
            # 'git svn info'.  If we fail because of an older git install,
            # here, figure out what version of git is installed and give
            # the user a hint about what to do next.
            version = execute(["git", "svn", "--version"], ignore_errors=True)
            version_parts = re.search('version (\d+)\.(\d+)\.(\d+)',
                                      version)
            svn_remote = execute(["git", "config", "--get",
                                  "svn-remote.svn.url"], ignore_errors=True)

            if (version_parts and
                not self.is_valid_version((int(version_parts.group(1)),
                                           int(version_parts.group(2)),
                                           int(version_parts.group(3))),
                                          (1, 5, 4)) and
                svn_remote):
                die("Your installation of git-svn must be upgraded to " + \
                    "version 1.5.4 or later")

        # Okay, maybe Perforce.
        # TODO

        # Nope, it's git then.
        origin = execute(["git", "remote", "show", "origin"])
        m = re.search(r'URL: (.+)', origin)
        if m:
            url = m.group(1).rstrip('/')
            if url:
                self.type = "git"
                return RepositoryInfo(path=url, base_path='',
                                      supports_parent_diffs=True)

        return None

    def is_valid_version(self, actual, expected):
        """
        Takes two tuples, both in the form:
            (major_version, minor_version, micro_version)
        Returns true if the actual version is greater than or equal to
        the expected version, and false otherwise.
        """
        return (actual[0] > expected[0]) or \
               (actual[0] == expected[0] and actual[1] > expected[1]) or \
               (actual[0] == expected[0] and actual[1] == expected[1] and \
                actual[2] >= expected[2])

    def scan_for_server(self, repository_info):
        # Scan first for dot files, since it's faster and will cover the
        # user's $HOME/.reviewboardrc
        server_url = super(GitClient, self).scan_for_server(repository_info)

        if server_url:
            return server_url

        # TODO: Maybe support a server per remote later? Is that useful?
        url = execute(["git", "config", "--get", "reviewboard.url"],
                      ignore_errors=True).strip()
        if url:
            return url

        if self.type == "svn":
            # Try using the reviewboard:url property on the SVN repo, if it
            # exists.
            prop = SVNClient().scan_for_server_property(repository_info)

            if prop:
                return prop

        return None

    def diff(self, args):
        """
        Performs a diff across all modified files in the branch, taking into
        account a parent branch.
        """
        parent_branch = options.parent_branch or "master"

        diff_lines = self.make_diff(parent_branch)

        if parent_branch != "master":
            parent_diff_lines = self.make_diff("master", parent_branch)
        else:
            parent_diff_lines = None

        if options.guess_summary and not options.summary:
            options.summary = execute(["git", "log", "--pretty=format:%s",
                                       "HEAD^.."], ignore_errors=True).strip()

        if options.guess_description and not options.description:
            options.description = execute(
                ["git", "log", "--pretty=format:%s%n%n%b", parent_branch + ".."],
                ignore_errors=True).strip()

        return (diff_lines, parent_diff_lines)

    def make_diff(self, parent_branch, source_branch=""):
        """
        Performs a diff on a particular branch range.
        """
        if self.type == "svn":
            diff_lines = execute(["git", "diff", "--no-color", "--no-prefix",
                                  "-r", "-u", "%s..%s" % (parent_branch,
                                                          source_branch)],
                                 split_lines=True)
            return self.make_svn_diff(parent_branch, diff_lines)
        elif self.type == "git":
            return execute(["git", "diff", "--no-color", "--full-index",
                            parent_branch])

        return None

    def make_svn_diff(self, parent_branch, diff_lines):
        """
        Formats the output of git diff such that it's in a form that
        svn diff would generate. This is needed so the SVNTool in Review
        Board can properly parse this diff.
        """
        rev = execute(["git", "svn", "find-rev", "master"]).strip()

        if not rev:
            return None

        diff_data = ""
        filename = ""
        revision = ""
        newfile = False

        for line in diff_lines:
            if line.startswith("diff "):
                # Grab the filename and then filter this out.
                # This will be in the format of:
                #
                # diff --git a/path/to/file b/path/to/file
                info = line.split(" ")
                diff_data += "Index: %s\n" % info[2]
                diff_data += "=" * 67
                diff_data += "\n"
            elif line.startswith("index "):
                # Filter this out.
                pass
            elif line.strip() == "--- /dev/null":
                # New file
                newfile = True
            elif line.startswith("--- "):
                newfile = False
                diff_data += "--- %s\t(revision %s)\n" % \
                             (line[4:].strip(), rev)
            elif line.startswith("+++ "):
                filename = line[4:].strip()
                if newfile:
                    diff_data += "--- %s\t(revision 0)\n" % filename
                    diff_data += "+++ %s\t(revision 0)\n" % filename
                else:
                    # We already printed the "--- " line.
                    diff_data += "+++ %s\t(working copy)\n" % filename
            else:
                diff_data += line

        return diff_data

    def diff_between_revisions(self, revision_range, args, repository_info):
        pass


SCMCLIENTS = (
    SVNClient(),
    CVSClient(),
    GitClient(),
    MercurialClient(),
    PerforceClient(),
    ClearCaseClient(),
)

def debug(s):
    """
    Prints debugging information if post-review was run with --debug
    """
    if DEBUG or options and options.debug:
        print ">>> %s" % s


def make_tempfile():
    """
    Creates a temporary file and returns the path. The path is stored
    in an array for later cleanup.
    """
    fd, tmpfile = mkstemp()
    os.close(fd)
    tempfiles.append(tmpfile)
    return tmpfile


def check_install(command):
    """
    Try executing an external command and return a boolean indicating whether
    that command is installed or not.  The 'command' argument should be
    something that executes quickly, without hitting the network (for
    instance, 'svn help' or 'git --version').
    """
    try:
        p = subprocess.Popen(command.split(' '),
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE)
        return True
    except OSError:
        return False


def execute(command, env=None, split_lines=False, ignore_errors=False,
            extra_ignore_errors=(), translate_newlines=True):
    """
    Utility function to execute a command and return the output.
    """
    if isinstance(command, list):
        debug(subprocess.list2cmdline(command))
    else:
        debug(command)

    if env:
        env.update(os.environ)
    else:
        env = os.environ.copy()

    env['LC_ALL'] = 'en_US.UTF-8'
    env['LANGUAGE'] = 'en_US.UTF-8'

    if sys.platform.startswith('win'):
        p = subprocess.Popen(command,
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT,
                             shell=False,
                             universal_newlines=translate_newlines,
                             env=env)
    else:
        p = subprocess.Popen(command,
                             stdin=subprocess.PIPE,
                             stdout=subprocess.PIPE,
                             stderr=subprocess.STDOUT,
                             shell=False,
                             close_fds=True,
                             universal_newlines=translate_newlines,
                             env=env)
    if split_lines:
        data = p.stdout.readlines()
    else:
        data = p.stdout.read()
    rc = p.wait()
    if rc and not ignore_errors and rc not in extra_ignore_errors:
        die('Failed to execute command: %s\n%s' % (command, data))

    return data


def die(msg=None):
    """
    Cleanly exits the program with an error message. Erases all remaining
    temporary files.
    """
    for tmpfile in tempfiles:
        try:
            os.unlink(tmpfile)
        except:
            pass

    if msg:
        print msg

    sys.exit(1)


def walk_parents(path):
    """
    Walks up the tree to the root directory.
    """
    while os.path.splitdrive(path)[1] != os.sep:
        yield path
        path = os.path.dirname(path)


def load_config_file(filename):
    """
    Loads data from a config file.
    """
    config = {
        'TREES': {},
    }

    if os.path.exists(filename):
        try:
            execfile(filename, config)
        except:
            pass

    return config


def tempt_fate(server, tool, changenum, diff_content=None,
               parent_diff_content=None, submit_as=None, retries=3):
    """
    Attempts to create a review request on a Review Board server and upload
    a diff. On success, the review request path is displayed.
    """
    try:
        save_draft = False

        if options.rid:
            review_request = server.get_review_request(options.rid)
        else:
            review_request = server.new_review_request(changenum, submit_as)

        if options.target_groups:
            server.set_review_request_field(review_request, 'target_groups',
                                            options.target_groups)
            save_draft = True

        if options.target_people:
            server.set_review_request_field(review_request, 'target_people',
                                            options.target_people)
            save_draft = True

        if options.summary:
            server.set_review_request_field(review_request, 'summary',
                                            options.summary)
            save_draft = True

        if options.branch:
            server.set_review_request_field(review_request, 'branch',
                                            options.branch)
            save_draft = True

        if options.bugs_closed:
            server.set_review_request_field(review_request, 'bugs_closed',
                                            options.bugs_closed)
            save_draft = True

        if options.description:
            server.set_review_request_field(review_request, 'description',
                                            options.description)
            save_draft = True

        if options.testing_done:
            server.set_review_request_field(review_request, 'testing_done',
                                            options.testing_done)
            save_draft = True

        if save_draft:
            server.save_draft(review_request)
    except APIError, e:
        rsp, = e.args
        if rsp['err']['code'] == 103: # Not logged in
            retries = retries - 1

            # We had an odd issue where the server ended up a couple of
            # years in the future. Login succeeds but the cookie date was
            # "odd" so use of the cookie appeared to fail and eventually
            # ended up at max recursion depth :-(. Check for a maximum
            # number of retries.
            if retries >= 0:
                server.login(force=True)
                tempt_fate(server, tool, changenum, diff_content,
                           parent_diff_content, submit_as, retries=retries)
                return

        if options.rid:
            die("Error getting review request %s: %s (code %s)" % \
                (options.rid, rsp['err']['msg'], rsp['err']['code']))
        else:
            die("Error creating review request: %s (code %s)" % \
                (rsp['err']['msg'], rsp['err']['code']))


    if not server.info.supports_changesets or not options.change_only:
        try:
            server.upload_diff(review_request, diff_content,
                               parent_diff_content)
        except APIError, e:
            rsp, = e.args
            print "Error uploading diff: %s (%s)" % (rsp['err']['msg'],
                                                     rsp['err']['code'])
            debug(rsp)
            die("Your review request still exists, but the diff is not " +
                "attached.")

    if options.publish:
        server.publish(review_request)

    request_url = 'r/' + str(review_request['id'])
    review_url = urljoin(server.url, request_url)

    if not review_url.startswith('http'):
        review_url = 'http://%s' % review_url

    print "Review request #%s posted." % (review_request['id'],)
    print
    print review_url

    return review_url


def parse_options(args):
    parser = OptionParser(usage="%prog [-pond] [-r review_id] [changenum]",
                          version="%prog " + VERSION)

    parser.add_option("-p", "--publish",
                      dest="publish", action="store_true", default=PUBLISH,
                      help="publish the review request immediately after "
                           "submitting")
    parser.add_option("-r", "--review-request-id",
                      dest="rid", metavar="ID", default=None,
                      help="existing review request ID to update")
    parser.add_option("-o", "--open",
                      dest="open_browser", action="store_true",
                      default=OPEN_BROWSER,
                      help="open a web browser to the review request page")
    parser.add_option("-n", "--output-diff",
                      dest="output_diff_only", action="store_true",
                      default=False,
                      help="outputs a diff to the console and exits. "
                           "Does not post")
    parser.add_option("--server",
                      dest="server", default=REVIEWBOARD_URL,
                      metavar="SERVER",
                      help="specify a different Review Board server "
                           "to use")
    parser.add_option("--diff-only",
                      dest="diff_only", action="store_true", default=False,
                      help="uploads a new diff, but does not update "
                           "info from changelist")
    parser.add_option("--target-groups",
                      dest="target_groups", default=TARGET_GROUPS,
                      help="names of the groups who will perform "
                           "the review")
    parser.add_option("--target-people",
                      dest="target_people", default=TARGET_PEOPLE,
                      help="names of the people who will perform "
                           "the review")
    parser.add_option("--summary",
                      dest="summary", default=None,
                      help="summary of the review ")
    parser.add_option("--description",
                      dest="description", default=None,
                      help="description of the review ")
    parser.add_option("--description-file",
                      dest="description_file", default=None,
                      help="text file containing a description of the review")
    parser.add_option("--guess-summary",
                      dest="guess_summary", action="store_true",
                      default=False,
                      help="guess summary from the latest commit (git/"
                           "hgsubversion only)")
    parser.add_option("--guess-description",
                      dest="guess_description", action="store_true",
                      default=False,
                      help="guess description based on commits on this branch "
                           "(git/hgsubversion only)")
    parser.add_option("--testing-done",
                      dest="testing_done", default=None,
                      help="details of testing done ")
    parser.add_option("--testing-done-file",
                      dest="testing_file", default=None,
                      help="text file containing details of testing done ")
    parser.add_option("--branch",
                      dest="branch", default=None,
                      help="affected branch ")
    parser.add_option("--bugs-closed",
                      dest="bugs_closed", default=None,
                      help="list of bugs closed ")
    parser.add_option("--revision-range",
                      dest="revision_range", default=None,
                      help="generate the diff for review based on given "
                           "revision range")
    parser.add_option("--label",
                      dest="label", default=None,
                      help="label (ClearCase Only) ")
    parser.add_option("--submit-as",
                      dest="submit_as", default=SUBMIT_AS, metavar="USERNAME",
                      help="user name to be recorded as the author of the "
                           "review request, instead of the logged in user")
    parser.add_option("--username",
                      dest="username", default=None, metavar="USERNAME",
                      help="user name to be supplied to the reviewboard server")
    parser.add_option("--password",
                      dest="password", default=None, metavar="PASSWORD",
                      help="password to be supplied to the reviewboard server")
    parser.add_option("--change-only",
                      dest="change_only", action="store_true",
                      default=False,
                      help="updates info from changelist, but does "
                           "not upload a new diff (only available if your "
                           "repository supports changesets)")
    parser.add_option("--parent",
                      dest="parent_branch", default=None,
                      metavar="PARENT_BRANCH",
                      help="the parent branch this diff should be against "
                           "(only available if your repository supports "
                           "parent diffs)")
    parser.add_option("--p4-client",
                      dest="p4_client", default=None,
                      help="the Perforce client name that the review is in")
    parser.add_option("--p4-port",
                      dest="p4_port", default=None,
                      help="the Perforce servers IP address that the review is on")
    parser.add_option("--repository-url",
                      dest="repository_url", default=None,
                      help="the url for a repository for creating a diff "
                           "outside of a working copy (currently only supported "
                           "by Subversion).  Requires --revision-range")
    parser.add_option("-d", "--debug",
                      action="store_true", dest="debug", default=DEBUG,
                      help="display debug output")

    (globals()["options"], args) = parser.parse_args(args)

    if options.description and options.description_file:
        sys.stderr.write("The --description and --description-file options "
                         "are mutually exclusive.\n")
        sys.exit(1)

    if options.description_file:
        if os.path.exists(options.description_file):
            fp = open(options.description_file, "r")
            options.description = fp.read()
            fp.close()
        else:
            sys.stderr.write("The description file %s does not exist.\n" %
                             options.description_file)
            sys.exit(1)

    if options.testing_done and options.testing_file:
        sys.stderr.write("The --testing-done and --testing-done-file options "
                         "are mutually exclusive.\n")
        sys.exit(1)

    if options.testing_file:
        if os.path.exists(options.testing_file):
            fp = open(options.testing_file, "r")
            options.testing_done = fp.read()
            fp.close()
        else:
            sys.stderr.write("The testing file %s does not exist.\n" %
                             options.testing_file)
            sys.exit(1)

    if options.repository_url and not options.revision_range:
        sys.stderr.write("The --repository-url option requires the "
                         "--revision-range option.\n")
        sys.exit(1)

    return args

def determine_client():

    repository_info = None
    tool = None

    # Try to find the SCM Client we're going to be working with.
    for tool in SCMCLIENTS:
        repository_info = tool.get_repository_info()

        if repository_info:
            break

    if not repository_info:
        if options.repository_url:
            print "No supported repository could be access at the supplied url."
        else:
            print "The current directory does not contain a checkout from a"
            print "supported source code repository."
        sys.exit(1)

    # Verify that options specific to an SCM Client have not been mis-used.
    if options.change_only and not repository_info.supports_changesets:
        sys.stderr.write("The --change-only option is not valid for the "
                         "current SCM client.\n")
        sys.exit(1)

    if options.parent_branch and not repository_info.supports_parent_diffs:
        sys.stderr.write("The --parent option is not valid for the "
                         "current SCM client.\n")
        sys.exit(1)

    if ((options.p4_client or options.p4_port) and \
        not isinstance(tool, PerforceClient)):
        sys.stderr.write("The --p4-client and --p4-port options are not valid "
                         "for the current SCM client.\n")
        sys.exit(1)

    return (repository_info, tool)

def main():
    if 'USERPROFILE' in os.environ:
        homepath = os.path.join(os.environ["USERPROFILE"], "Local Settings",
                                "Application Data")
    elif 'HOME' in os.environ:
        homepath = os.environ["HOME"]
    else:
        homepath = ''

    # Load the config and cookie files
    globals()['user_config'] = \
        load_config_file(os.path.join(homepath, ".reviewboardrc"))
    cookie_file = os.path.join(homepath, ".post-review-cookies.txt")

    args = parse_options(sys.argv[1:])

    repository_info, tool = determine_client()

    # Try to find a valid Review Board server to use.
    if options.server:
        server_url = options.server
    else:
        server_url = tool.scan_for_server(repository_info)

    if not server_url:
        print "Unable to find a Review Board server for this source code tree."
        sys.exit(1)

    server = ReviewBoardServer(server_url, repository_info, cookie_file)

    if repository_info.supports_changesets:
        changenum = tool.get_changenum(args)
    else:
        changenum = None

    if options.revision_range:
        diff = tool.diff_between_revisions(options.revision_range, args,
                                           repository_info)
        parent_diff = None
    elif options.label and isinstance(tool, ClearCaseClient):
        diff, parent_diff = tool.diff_label(options.label)
    else:
        diff, parent_diff = tool.diff(args)

    if options.output_diff_only:
        print diff
        sys.exit(0)

    # Let's begin.
    server.login()

    review_url = tempt_fate(server, tool, changenum, diff_content=diff,
                            parent_diff_content=parent_diff,
                            submit_as=options.submit_as)

    # Load the review up in the browser if requested to:
    if options.open_browser:
        try:
            import webbrowser
            if 'open_new_tab' in dir(webbrowser):
                # open_new_tab is only in python 2.5+
                webbrowser.open_new_tab(review_url)
            elif 'open_new' in dir(webbrowser):
                webbrowser.open_new(review_url)
            else:
                os.system( 'start %s' % review_url )
        except:
            print 'Error opening review URL: %s' % review_url


if __name__ == "__main__":
    main()
