// © 2009–2010 EPFL/LAMP
// code by Gilles Dubochet with contributions by Pedro Furlanetto

$(document).ready(function(){
    var isHiddenClass;
    if (document.title == 'scala.AnyRef') {
        isHiddenClass = function (name) {
            return name == 'scala.Any';
        };
    } else {
        isHiddenClass = function (name) {
            return name == 'scala.Any' || name == 'scala.AnyRef';
        };
    }

    $("#linearization li").filter(function(){
        return isHiddenClass($(this).attr("name"));
    }).removeClass("in").addClass("out");
    filter();

    var input = $("#textfilter input");
    input.bind("keyup", function(event) {
        if (event.keyCode == 27) { // escape
            input.attr("value", "");
        }
        filter();
    });
    input.focus(function(event) { input.select(); });
    $("#textfilter > .post").click(function(){
        $("#textfilter input").attr("value", "");
        filter();
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

/*
    $("#ancestors > ol > li.hideall").click(function() {
        $("#linearization li.in").removeClass("in").addClass("out");
        $("#linearization li:first").removeClass("out").addClass("in");
        filter();
    })
    $("#ancestors > ol > li.showall").click(function() {
        var filtered =
            $("#linearization li.out").filter(function() {
                return ! isHiddenClass($(this).attr("name"));
            });
        filtered.removeClass("out").addClass("in");
        filter();
    });
*/


    $("#ancestors > ol > li.hideall").click(function() {
        if ($(this).hasClass("out")) {
            $(this).removeClass("out").addClass("in");
            $("#ancestors > ol > li.showall").removeClass("in").addClass("out");
            $("#linearization li.in").removeClass("in").addClass("out");
            $("#linearization li:first").removeClass("out").addClass("in");
            filter();
        };
    })
    $("#ancestors > ol > li.showall").click(function() {
        if($(this).hasClass("out")){
            $(this).removeClass("out").addClass("in");
            $("#ancestors > ol > li.hideall").removeClass("in").addClass("out");
            var filtered =
                $("#linearization li.out").filter(function() {
                    return ! isHiddenClass($(this).attr("name"));
                });
            filtered.removeClass("out").addClass("in");
            filter();
        };
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
    //http://flowplayer.org/tools/tooltip.html
    $(".extype").tooltip({
        tip: "#tooltip",
        position:"top center",
        predelay: 500,
        onBeforeShow: function(ev) {
            $(this.getTip()).text(this.getTrigger().attr("name"));
        }
    });
    $(".defval").tooltip({
        tip: "#tooltip",
        position:"top center",
        predelay: 500,        
        onBeforeShow: function(ev) {
            $(this.getTip()).html(this.getTrigger().attr("name"))
        }        
    });   

    /* Add toggle arrows */
    var docAllSigs = $("#template li").has(".fullcomment").find(".signature");
    
    function commentToggleFct(signature){
        var parent = signature.parent();
        var shortComment = $(".shortcomment", parent);
        var fullComment = $(".fullcomment", parent);
        var vis = $(":visible", fullComment);
        signature.toggleClass("closed").toggleClass("opened");
        if (vis.length > 0) {
            shortComment.slideDown(50);
            fullComment.slideUp(50);
            signature.addClass("closed");
            signature.removeClass("opened");
        }
        else {
            shortComment.slideUp(50);
            fullComment.slideDown(50);
            signature.removeClass("closed");
            signature.addClass("opened");
        }
    };
    docAllSigs.addClass("closed");
    docAllSigs.click(function() {
        commentToggleFct($(this));
    });
    
    /* Linear super types and known subclasses */
    function toggleShowContentFct(outerElement){
      var content = $(".hiddenContent", outerElement);
      var vis = $(":visible", content);
      if (vis.length > 0) {
        content.slideUp(100);
        $(".showElement", outerElement).show();
        $(".hideElement", outerElement).hide();
      }
      else {
        content.slideDown(100);
        $(".showElement", outerElement).hide();
        $(".hideElement", outerElement).show();
      }
    };
    $(".toggleContainer").click(function() {
      toggleShowContentFct($(this));
    });
});

function orderAlpha() {
    $("#template > div.parent").hide();
    $("#ancestors").show();
    filter();
};

function orderInherit() {
    $("#template > div.parent").show();
    $("#ancestors").hide();
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
    $("#template > div.parent").each(function(){
        parents[$(this).attr("name")] = $(this);
    });
    // 
    $("#types > ol > li").each(function(){
        var qualName = $(this).attr("name");
        var owner = qualName.slice(0, qualName.indexOf("#"));
        var name = qualName.slice(qualName.indexOf("#") + 1);
        var parent = parents[owner];
        if (parent != undefined) {
            var types = $("> .types > ol", parent);
            if (types.length == 0) {
                parent.append("<div class='types members'><h3>Type Members</h3><ol></ol></div>");
                types = $("> .types > ol", parent);
            }
            types.append($(this).clone());
        }
    });
    $("#values > ol > li").each(function(){
        var qualName = $(this).attr("name");
        var owner = qualName.slice(0, qualName.indexOf("#"));
        var name = qualName.slice(qualName.indexOf("#") + 1);
        var parent = parents[owner];
        if (parent != undefined) {
            var values = $("> .values > ol", parent);
            if (values.length == 0) {
                parent.append("<div class='values members'><h3>Value Members</h3><ol></ol></div>");
                values = $("> .values > ol", parent);
            }
            values.append($(this).clone());
        }
    });
    $("#template > div.parent").each(function(){
        if ($("> div.members", this).length == 0) { $(this).remove(); };
    });
    $("#template > div.parent").each(function(){
        $(this).hide();
    });
};

function filter() {
    var query = $("#textfilter input").attr("value").toLowerCase();
    var queryRegExp = new RegExp(query, "i");
    var inheritHides = null
    if ($("#order > ol > li.inherit").hasClass("in")) {
        inheritHides = $("#linearization > li:gt(0)");
    }
    else {
        inheritHides = $("#linearization > li.out");
    }
    var outOwners =
        inheritHides.map(function(){
            var r = $(this).attr("name");
            return r
        }).get();
    var prtVisbl = $("#visbl > ol > li.all").hasClass("in");
    $(".members > ol > li").each(function(){
        var vis1 = $(this).attr("visbl");
        var qualName1 = $(this).attr("name");
        //var name1 = qualName1.slice(qualName1.indexOf("#") + 1);
        var showByOwned = true;
        if ($(this).parents(".parent").length == 0) {
            // owner filtering must not happen in "inherited from" member lists
            var ownerIndex = qualName1.indexOf("#");
            if (ownerIndex < 0) { ownerIndex = qualName1.lastIndexOf("."); }
            var owner1 = qualName1.slice(0, ownerIndex);
            for (out in outOwners) {
                if (outOwners[out] == owner1) {
                    showByOwned = false;
                };
            };
        };
        var showByVis = true;
        if (vis1 == "prt") {
            showByVis = prtVisbl;
        };
        var showByName = true;
        if (query != "") {
            var content = $(this).attr("name") + $("> .fullcomment .cmt", this).text();
            showByName = queryRegExp.test(content);
        };
        if (showByOwned && showByVis && showByName) {
          $(this).show();
        }
        else {
          $(this).hide();
        };
    });
    $(".members").each(function(){
        $(this).show();
        if ($(" > ol > li:visible", this).length == 0) { $(this).hide(); }
    });
    return false
};

function windowTitle()
{
    parent.document.title=document.title;
};
