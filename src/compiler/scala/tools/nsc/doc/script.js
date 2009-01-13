<!--
// NSC -- new Scala compiler
// Copyright 2005-2009 LAMP/EPFL
// @author  Stephane Micheloud

// $Id$

function setWindowTitle(title) {
  parent.document.title = title;
}

var java_api_root = 'http://java.sun.com/javase/6/docs/api/';
//var java_api_root = 'http://java.sun.com/j2se/1.5.0/docs/api/';
//var java_api_root = 'http://lamp.epfl.ch/~linuxsoft/java/jdk1.5/docs/api/';

var javax_servlet_api_root = 'http://java.sun.com/products/servlet/2.3/javadoc/';

var scala_doc_url = parent.document.URL;
var scala_api_root = scala_doc_url.substring(0, scala_doc_url.lastIndexOf("/")+1);

var ant_api_root = 'http://lamp.epfl.ch/~linuxsoft/ant/manual/api/';
//var ant_api_root = 'http://www.net-freaks.org/doc/ant-1.6.5/manual/api/';

var eclipse_api_root = 'http://help.eclipse.org/help32/topic/org.eclipse.platform.doc.isv/reference/api/';

var fjbg_api_root = 'http://lamp.epfl.ch/~linuxsoft/fjbg/api/';

var liftweb_api_root = 'http://lamp.epfl.ch/~linuxsoft/liftweb/apidocs/';

function get_api_root(key) {
  root = null;
  if      (key.indexOf("ch/epfl/lamp/fjbg/") == 0) { root = fjbg_api_root; }
  else if (key.indexOf("java/"             ) == 0) { root = java_api_root; }
  else if (key.indexOf("javax/"            ) == 0) { root = java_api_root; }
  else if (key.indexOf("javax/servlet/"    ) == 0) { root = javax_servlet_api_root; }
  else if (key.indexOf("scala/"            ) == 0) { root = scala_api_root; }
  else if (key.indexOf("org/apache/tools/" ) == 0) { root = ant_api_root; }
  else if (key.indexOf("org/eclipse/"      ) == 0) { root = eclipse_api_root; }
  else if (key.indexOf("net/liftweb/"      ) == 0) { root = liftweb_api_root; }
  return root;
}

var scala_src_root = 'http://lampsvn.epfl.ch/trac/scala/browser/scala/trunk/';
var lib_src_root = scala_src_root + 'src/library/';
var comp_src_root = scala_src_root + 'src/compiler/';
var actors_src_root = scala_src_root + 'src/actors/';
var dbc_src_root = scala_src_root + 'src/dbc/';
var swing_src_root = scala_src_root + 'src/swing/';

//var liftweb_src_root = 'http://liftweb.googlecode.com/svn/trunk/liftweb/lift/src/main/scala/';

function get_src_root(key) {
  root = null;
  if      (key.indexOf("scala/actors/") == 0) { root = actors_src_root; }
  else if (key.indexOf("scala/dbc/"   ) == 0) { root = dbc_src_root; }
  else if (key.indexOf("scala/swing/" ) == 0) { root = swing_src_root; }
  else if (key.indexOf("scala/tools/" ) == 0) { root = comp_src_root; }
  else if (key.indexOf("scala/"       ) == 0) { root = lib_src_root; }
  //else if (key.indexOf("net/liftweb/" ) == 0) { root = liftweb_src_root; }
  return root;
}

function init() {
  elems = document.getElementsByTagName('a');
  for (i = 0; i < elems.length; i++) {
    try {
      key = elems[i].getAttribute('class');
      href = elems[i].getAttribute('href');
      api_root = get_api_root(key);
      if (api_root != null) {
        href1 = href.substring(href.lastIndexOf("#"))
        value = api_root + key + ".html" + href1;
        elems[i].setAttribute('href', value);
      }
      src_root = get_src_root(key);
      if (src_root != null) {
        value = src_root + key + '.scala?view=markup';
        elems[i].setAttribute('href', value);
        elems[i].setAttribute('target' , '_top');
      }
    }
    catch (e) {
      // do nothing
    }
  }
}

function getLocation() {
  kinds = parent.navigationFrame.document.getElementById("kinds");
  oldLocation = parent.classesFrame.window.location.href;
  //alert("oldLocation="+oldLocation);
  pos = oldLocation.lastIndexOf("#");
  classesURL = (pos > 0) ? oldLocation.substring(0, pos) : oldLocation;
  newLocation = classesURL + kinds.options[kinds.selectedIndex].value;
  //alert("newLocation="+newLocation);
  return newLocation;
}

function gotoKind() {
  parent.classesFrame.window.location = getLocation();
}

function resetKind() {
  kinds = parent.navigationFrame.document.getElementById("kinds");
  kinds.selectedIndex = 0;
}

function gotoName(letter) {
  parent.classesFrame.window.location = getLocation() + "_" + letter;
}
-->

