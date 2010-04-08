// © 2009–2010 EPFL/LAMP
// code by Gilles Dubochet with contributions by Pedro Furlanetto

$(document).ready(function(){
    var prefilters = $("#ancestors > ol > li").filter(function(){
        var name = $(this).attr("name");
        return name == "scala.Any" || name == "scala.AnyRef";
    });
    prefilters.removeClass("in");
    prefilters.addClass("out");
    filter();
    $("#ancestors > ol > li").click(function(){
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
    $("#ancestors > ol > li.hideall").click(function() {
        $("#ancestors > ol > li.in").removeClass("in").addClass("out");
        filter();
    })
    $("#ancestors > ol > li.showall").click(function() {
        var filtered =
            $("#ancestors > ol > li.out").filter(function() {
                var name = $(this).attr("name");
                return !(name == "scala.Any" || name == "scala.AnyRef");
            });
        filtered.removeClass("out").addClass("in");
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
    //http://flowplayer.org/tools/tooltip.html
    $(".extype").tooltip({
        tip: "#tooltip",
        position:"top center",        
        onBeforeShow: function(ev) {
            $(this.getTip()).text(this.getTrigger().attr("name"));
        }        
    });
    $("#template div.fullcomment").hide();
    var docAllSigs = $("#template .signature");
    function commentShowFct(fullComment){
        var vis = $(":visible", fullComment);
        if (vis.length > 0) {
            fullComment.slideUp(100);
        }
        else {
            fullComment.slideDown(100);
        }
    };
    var docShowSigs = docAllSigs.filter(function(){
        return $("+ div.fullcomment", $(this)).length > 0;
    });
    docShowSigs.css("cursor", "help");
    docShowSigs.click(function(){
        commentShowFct($("+ div.fullcomment", $(this)));
    });
    function commentToggleFct(shortComment){
        var vis = $("~ div.fullcomment:visible", shortComment);
        if (vis.length > 0) {
            shortComment.slideDown(100);
            vis.slideUp(100);
        }
        else {
            var hid = $("~ div.fullcomment:hidden", shortComment);
            hid.slideDown(100);
            shortComment.slideUp(100);
        }
    };
    var docToggleSigs = docAllSigs.filter(function(){
        return $("+ p.shortcomment", $(this)).length > 0;
    });
    docToggleSigs.css("cursor", "help");
    docToggleSigs.click(function(){
        commentToggleFct($("+ p.shortcomment", $(this)));
    });
    $("p.shortcomment").click(function(){
        commentToggleFct($(this));
    });
});

function filter() {
    var outOwners =
        $("#mbrsel ol#linearization > li.out").map(function(){
            var r = $(this).attr("name");
            return r
        }).get();
    var prtVisbl = $("#visbl > ol > li.all").hasClass("in");
    $(".members > ol > li").each(function(){
        var vis1 = $(this).attr("visbl");
        var qualName1 = $(this).attr("name");
        var owner1 = qualName1.slice(0, qualName1.indexOf("#"));
        //var name1 = qualName1.slice(qualName1.indexOf("#") + 1);
        var showByOwned = true;
        for (out in outOwners) {
            if (outOwners[out] == owner1) {
                showByOwned = false;
            };
        };
        var showByVis = true
        if (vis1 == "prt") {
            showByVis = prtVisbl;
        };
        if (showByOwned && showByVis) {
          $(this).show();
        }
        else {
          $(this).hide();
        };
    });
    return false
};
