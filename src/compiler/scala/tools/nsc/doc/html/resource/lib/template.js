// © 2009–2010 EPFL/LAMP
// code by Gilles Dubochet with contributions by Pedro Furlanetto

$(document).ready(function(){
    var isHiddenClass = function (name) {
        return name == 'scala.Any' ||
               name == 'scala.AnyRef' ||
               name == 'scala.Predef.any2stringfmt' ||
               name == 'scala.Predef.any2stringadd' ||
               name == 'scala.Predef.any2ArrowAssoc' ||
               name == 'scala.Predef.any2Ensuring' ||
               name == 'scala.collection.TraversableOnce.alternateImplicit'
    };

    $("#linearization li:gt(0)").filter(function(){
        return isHiddenClass($(this).attr("name"));
    }).removeClass("in").addClass("out");

    $("#implicits li").filter(function(){
        return isHiddenClass($(this).attr("name"));
    }).removeClass("in").addClass("out");

    // Pre-filter members
    filter();

    // Member filter box
    var input = $("#textfilter input");
    input.bind("keyup", function(event) {

        switch ( event.keyCode ) {

        case 27: // escape key
            input.val("");
            filter(true);
            break;

        case 38: // up
            input.val("");
            filter(false);
            window.scrollTo(0, $("body").offset().top);
            input.focus();
            break;

        case 33: //page up
            input.val("");
            filter(false);            
            break;

        case 34: //page down
            input.val("");
            filter(false);            
            break;            

        default: 
            window.scrollTo(0, $("#mbrsel").offset().top);
            filter(true);        
            break;

        }        
    });
    input.focus(function(event) {
        input.select();
    });
    $("#textfilter > .post").click(function() {
        $("#textfilter input").attr("value", "");
        filter();
    });
    $(document).keydown(function(event) {

        if (event.keyCode == 9) { // tab
            $("#index-input", window.parent.document).focus();
            input.attr("value", "");
            return false;            
        }
    });

    $("#linearization li").click(function(){
        if ($(this).hasClass("in")) {
            $(this).removeClass("in");
            $(this).addClass("out");
        }
        else if ($(this).hasClass("out")) {
            $(this).removeClass("out");
            $(this).addClass("in");
        };
        filter();
    });

    $("#implicits li").click(function(){
        if ($(this).hasClass("in")) {
            $(this).removeClass("in");
            $(this).addClass("out");
        }
        else if ($(this).hasClass("out")) {
            $(this).removeClass("out");
            $(this).addClass("in");
        };
        filter();
    });

    $("#mbrsel > div[id=ancestors] > ol > li.hideall").click(function() {
        $("#linearization li.in").removeClass("in").addClass("out");
        $("#linearization li:first").removeClass("out").addClass("in");
        $("#implicits li.in").removeClass("in").addClass("out");
        filter();
    })
    $("#mbrsel > div[id=ancestors] > ol > li.showall").click(function() {
        var filteredLinearization =
            $("#linearization li.out").filter(function() {
                return ! isHiddenClass($(this).attr("name"));
            });
        filteredLinearization.removeClass("out").addClass("in");

        var filteredImplicits =
        $("#implicits li.out").filter(function() {
            return ! isHiddenClass($(this).attr("name"));
        });
        filteredImplicits.removeClass("out").addClass("in");

        filter();
    });
    $("#visbl > ol > li.public").click(function() {
        if ($(this).hasClass("out")) {
            $(this).removeClass("out").addClass("in");
            $("#visbl > ol > li.all").removeClass("in").addClass("out");
            filter();
        };
    })
    $("#visbl > ol > li.all").click(function() {
        if ($(this).hasClass("out")) {
            $(this).removeClass("out").addClass("in");
            $("#visbl > ol > li.public").removeClass("in").addClass("out");
            filter();
        };
    });
    $("#order > ol > li.alpha").click(function() {
        if ($(this).hasClass("out")) {
            $(this).removeClass("out").addClass("in");
            $("#order > ol > li.inherit").removeClass("in").addClass("out");
            orderAlpha();
        };
    })
    $("#order > ol > li.inherit").click(function() {
        if ($(this).hasClass("out")) {
            $(this).removeClass("out").addClass("in");
            $("#order > ol > li.alpha").removeClass("in").addClass("out");
            orderInherit();
        };
    });
    initInherit();

    // Create tooltips
    $(".extype").add(".defval").tooltip({
        tip: "#tooltip",
        position:"top center",
        predelay: 500,
        onBeforeShow: function(ev) {
            $(this.getTip()).text(this.getTrigger().attr("name"));
        }
    });

    /* Add toggle arrows */
    //var docAllSigs = $("#template li").has(".fullcomment").find(".signature");
    // trying to speed things up a little bit
    var docAllSigs = $("#template li[fullComment=yes] .signature");

    function commentToggleFct(signature){
        var parent = signature.parent();
        var shortComment = $(".shortcomment", parent);
        var fullComment = $(".fullcomment", parent);
        var vis = $(":visible", fullComment);
        signature.toggleClass("closed").toggleClass("opened");
        if (vis.length > 0) {
            shortComment.slideDown(100);
            fullComment.slideUp(100);
        }
        else {
            shortComment.slideUp(100);
            fullComment.slideDown(100);
        }
    };
    docAllSigs.addClass("closed");
    docAllSigs.click(function() {
        commentToggleFct($(this));
    });

    /* Linear super types and known subclasses */
    function toggleShowContentFct(e){
      e.toggleClass("open");
      var content = $(".hiddenContent", e.parent().get(0));
      if (content.is(':visible')) {
        content.slideUp(100);
      }
      else {
        content.slideDown(100);
      }
    };

    $(".toggle:not(.diagram-link)").click(function() {
      toggleShowContentFct($(this));
    });

    // Set parent window title
    windowTitle();
});

function orderAlpha() {
    $("#template > div.parent").hide();
    $("#template > div.conversion").hide();
    $("#mbrsel > div[id=ancestors]").show();
    filter();
};

function orderInherit() {
    $("#template > div.parent").show();
    $("#template > div.conversion").show();
    $("#mbrsel > div[id=ancestors]").hide();
    filter();
};

/** Prepares the DOM for inheritance-based display. To do so it will:
  *  - hide all statically-generated parents headings;
  *  - copy all members from the value and type members lists (flat members) to corresponding lists nested below the
  *    parent headings (inheritance-grouped members);
  *  - initialises a control variable used by the filter method to control whether filtering happens on flat members
  *    or on inheritance-grouped members. */
function initInherit() {
    // parents is a map from fully-qualified names to the DOM node of parent headings.
    var parents = new Object();
    $("#inheritedMembers > div.parent").each(function(){
        parents[$(this).attr("name")] = $(this);
    });
    $("#inheritedMembers > div.conversion").each(function(){
        parents[$(this).attr("name")] = $(this);
    });
    $("#types > ol > li").each(function(){
        var mbr = $(this);
        this.mbrText = mbr.find("> .fullcomment .cmt").text();
        var qualName = mbr.attr("name");
        var owner = qualName.slice(0, qualName.indexOf("#"));
        var name = qualName.slice(qualName.indexOf("#") + 1);
        var parent = parents[owner];
        if (parent != undefined) {
            var types = $("> .types > ol", parent);
            if (types.length == 0) {
                parent.append("<div class='types members'><h3>Type Members</h3><ol></ol></div>");
                types = $("> .types > ol", parent);
            }
            var clone = mbr.clone();
            clone[0].mbrText = this.mbrText;
            types.append(clone);
        }
    });
    $("#values > ol > li").each(function(){
        var mbr = $(this);
        this.mbrText = mbr.find("> .fullcomment .cmt").text();
        var qualName = mbr.attr("name");
        var owner = qualName.slice(0, qualName.indexOf("#"));
        var name = qualName.slice(qualName.indexOf("#") + 1);
        var parent = parents[owner];
        if (parent != undefined) {
            var values = $("> .values > ol", parent);
            if (values.length == 0) {
                parent.append("<div class='values members'><h3>Value Members</h3><ol></ol></div>");
                values = $("> .values > ol", parent);
            }
            var clone = mbr.clone();
            clone[0].mbrText = this.mbrText;
            values.append(clone);
        }
    });
    $("#inheritedMembers > div.parent").each(function() {
        if ($("> div.members", this).length == 0) { $(this).remove(); };
    });
    $("#inheritedMembers > div.conversion").each(function() {
        if ($("> div.members", this).length == 0) { $(this).remove(); };
    });
};

/* filter used to take boolean scrollToMember */
function filter() {
    var query = $.trim($("#textfilter input").val()).toLowerCase();
    query = query.replace(/[-[\]{}()*+?.,\\^$|#]/g, "\\$&").replace(/\s+/g, "|");
    var queryRegExp = new RegExp(query, "i");
    var privateMembersHidden = $("#visbl > ol > li.public").hasClass("in");
    var orderingAlphabetic = $("#order > ol > li.alpha").hasClass("in");
    var hiddenSuperclassElementsLinearization = orderingAlphabetic ? $("#linearization > li.out") : $("#linearization > li:gt(0)");
    var hiddenSuperclassesLinearization = hiddenSuperclassElementsLinearization.map(function() {
      return $(this).attr("name");
    }).get();
    var hiddenSuperclassElementsImplicits = orderingAlphabetic ? $("#implicits > li.out") : $("#implicits > li");
    var hiddenSuperclassesImplicits = hiddenSuperclassElementsImplicits.map(function() {
      return $(this).attr("name");
    }).get();

    var hideInheritedMembers;

    if(orderingAlphabetic) {
      $("#inheritedMembers").hide();
      hideInheritedMembers = true;
      $("#allMembers > .members").each(filterFunc);
    }
    else {
      $("#inheritedMembers").show();
      hideInheritedMembers = true;
      $("#allMembers > .members").each(filterFunc);
      hideInheritedMembers = false;
      $("#inheritedMembers > .parent > .members").each(filterFunc);
      $("#inheritedMembers > .conversion > .members").each(filterFunc);
    }


    function filterFunc() {
      var membersVisible = false;
      var members = $(this);
      members.find("> ol > li").each(function() {
        var mbr = $(this);
        if (privateMembersHidden && mbr.attr("visbl") == "prt") {
          mbr.hide();
          return;
        }
        var name = mbr.attr("name");
        // Owner filtering must not happen in "inherited from" member lists
        if (hideInheritedMembers) {
          var ownerIndex = name.indexOf("#");
          if (ownerIndex < 0) {
            ownerIndex = name.lastIndexOf(".");
          }
          var owner = name.slice(0, ownerIndex);
          for (var i = 0; i < hiddenSuperclassesLinearization.length; i++) {
            if (hiddenSuperclassesLinearization[i] == owner) {
              mbr.hide();
              return;
            }
          };
          for (var i = 0; i < hiddenSuperclassesImplicits.length; i++) {
            if (hiddenSuperclassesImplicits[i] == owner) {
              mbr.hide();
              return;
            }
          };
        }
        if (query && !(queryRegExp.test(name) || queryRegExp.test(this.mbrText))) {
          mbr.hide();
          return;
        }
        mbr.show();
        membersVisible = true;
      });

      if (membersVisible)
        members.show();
      else
        members.hide();
    };

    return false;
};

function windowTitle()
{
    try {
        parent.document.title=document.title;
    }
    catch(e) {
      // Chrome doesn't allow settings the parent's title when
      // used on the local file system.
    }
};
