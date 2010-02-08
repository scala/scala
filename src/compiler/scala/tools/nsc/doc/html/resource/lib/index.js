// © 2009–2010 EPFL/LAMP
// code by Gilles Dubochet with contributions by Johannes Rudolph and "spiros"

var topLevelTemplates = null;
var topLevelPackages = null;

var scheduler = undefined;

$(document).ready(function() {

    scheduler = new Scheduler();
    scheduler.addLabel("init", 5);
    scheduler.addLabel("focus", 7);
    scheduler.addLabel("filter", 10);

 	// Saves a pointer to top-level templates and packages (used to regain top-level focus after removing
 	// a focusing filter).
    topLevelTemplates = $("#tpl > ol.templates").clone();
    topLevelPackages = $("#tpl > ol.packages").clone();

    // Hides unavailable tools
    $("#focusfilter").hide();

    configureTextFilter();
    configureEntityList();

});

function configureEntityList() {
	resizeFilterBlock();
    redirectEntityLinks();
    setEntityIcons();
    configureHideFilter();
    configureFocusFilter();
	resizeFilterBlock();
    textFilter();
}

// Configure behaviour: links open in right frame
function redirectEntityLinks() {
	$(".wu").each(function() {
        scheduler.add("init", this, function() {
            $("> h3 a.tplshow", this).add("> .templates a.tplshow", this).attr("target", "template");
        });
    });
}

// Configure layout: replace text by icons when necessary
function setEntityIcons() {
    var classIcon = $("#library img.class");
    var traitIcon = $("#library img.trait");
    var objectIcon = $("#library img.object");
    var packageIcon = $("#library img.package");
	$("#tpl ol.templates").each(function() {
        scheduler.add("init", this, function() {
            $("> li span.class", this).each(function() { $(this).replaceWith(classIcon.clone()); });
            $("> li span.trait", this).each(function() { $(this).replaceWith(traitIcon.clone()); });
            $("> li span.object", this).each(function() { $(this).replaceWith(objectIcon.clone()); });
            $("> li span.package", this).each(function() { $(this).replaceWith(packageIcon.clone()); });
        });
	});
}

// Configures behaviour: text filter filters
function configureTextFilter() {
    scheduler.add("init", this, function() {
        $("#textfilter input").bind("keyup", function(event) {
            if (event.keyCode == 27) { // escape
                $("#textfilter input").attr("value", "");
            }
            textFilter();
        });
        $("#textfilter input").focus(function(event) {
            $("#textfilter input").select();
        });
	});
}

// Filters all focused templates and packages. This function should be made less-blocking.
//   @param query The string of the query
function textFilter() {
    scheduler.clear("filter");
    scheduler.add("filter", this, function() {
        var query = $("#textfilter input").attr("value")
        var queryRegExp;
        if (query.toLowerCase() != query)
            // Regexp that matches CamelCase subbits: "BiSe" is
            // "[a-z]*Bi[a-z]*Se" and matches "BitSet", "ABitSet", ...
            queryRegExp = new RegExp(query.replace(/([A-Z])/g,"[a-z]*$1"));
        else
            // if query is all lower case make a normal case insensitive search
            queryRegExp = new RegExp(query, "i");

        $(".wu").each(function() {
            var pack = $(this);
            scheduler.add("filter", this, function() {
                $("> ol.templates > li", pack).each(function(){
                    var item = $(this).attr("title");
                    if (item == "" || queryRegExp.test(item)) {
                        $(this).show();
                        $(this).removeClass("hide");
                    }
                    else {
                        $(this).addClass("hide");
                        $(this).hide();
                    }
                });
                if ($("> ol > li:not(.hide)", pack).length > 0) {
                    pack.show();
                    pack.removeClass("hide");
                }
                else {
                    pack.addClass("hide");
                    pack.hide();
                };
                if ($("> ol.templates > li:not(.hide)", pack).length > 0) {
                    $("> h3", pack).show();
                    $("> .packhide", pack).show();
                    $("> .packfocus", pack).show();
                }
                else {
                    $("> h3", pack).hide();
                    $("> .packhide", pack).hide();
                    $("> .packfocus", pack).hide();
                };
            });
        });

    });
}

// Configure behaviour and layout: add focus and hide links on packages
function configureHideFilter() {
    scheduler.add("init", this, function() {
        $("#tpl .packages > li").prepend("<a class='packhide'>hide</a>");

        // Configures the hide links of packages
        $("#tpl .packages > li > a.packhide").click(function(event){
            var action = $(this).text();
            if (action == "hide") {
                $("~ ol", $(this)).hide();
                $(this).text("show");
            }
            else {
                $("~ ol", $(this)).show();
                $(this).text("hide");
            }
            return false;
        });
    });
}

function configureFocusFilter() {
    scheduler.add("init", this, function() {
        // Configures the focus links of packages
        $("#focusfilter .focusremove").replaceWith("<img class='focusremove icon' src='lib/remove.png'/>");
        $("#tpl .packages > li").prepend("<a class='packfocus'>focus</a>");
        $("#tpl .packages > li > a.packfocus").click(function(event){
            focusFilter($(this).parent());
            return false;
        });
        $("#focusfilter .focusremove").click(function(event){
            scheduler.clear("filter");
            $("#tpl > ol.templates").replaceWith(topLevelTemplates.clone());
            $("#tpl > ol.packages").replaceWith(topLevelPackages.clone());
            $("#focusfilter").hide();
            configureEntityList();
            return false;
        });
    });
}

// Focuses the entity index on a specific package. To do so, it will copy the sub-templates and sub-packages of the
// focuses package into the top-level templates and packages position of the index. The original top-level
//   @param package The <li> element that corresponds to the package in the entity index
function focusFilter(package) {
    scheduler.add("focus", this, function() {

        scheduler.clear("filter");

        var currentFocus = package.attr("title");
        
        $("#focusfilter .focuscoll").empty();
        $("#focusfilter .focuscoll").append(currentFocus);

        var templates = $(" > ol.templates", package);
        var packages = $(" > ol.packages", package);

        $("#tpl > ol.templates").replaceWith(templates);
        $("#tpl > ol.packages").replaceWith(packages);

        $("#focusfilter").show();
	    resizeFilterBlock();
    });
}

function resizeFilterBlock() {
    scheduler.add("init", this, function() {
        $("#tpl").css("top", $("#filter").outerHeight(true));
    });
}
