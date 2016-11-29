/**
 * JavaScript functions enhancing the SVG diagrams.
 *
 * @author Damien Obrist
 */

var diagrams = {};

/**
 * Initializes the diagrams in the main window.
 */
$(document).ready(function()
{
	// hide diagrams in browsers not supporting SVG
	if(Modernizr && !Modernizr.inlinesvg)
		return;

	if($("#content-diagram").length)
		$("#inheritance-diagram").css("padding-bottom", "20px");

	$(".diagram-container").css("display", "block");

	$(".diagram").each(function() {
		// store initial dimensions
		$(this).data("width", $("svg", $(this)).width());
		$(this).data("height", $("svg", $(this)).height());
		// store unscaled clone of SVG element
		$(this).data("svg", $(this).get(0).childNodes[0].cloneNode(true));
	});

	// make diagram visible, hide container
	$(".diagram").css("display", "none");
	$(".diagram svg").css({
		"position": "static",
		"visibility": "visible",
		"z-index": "auto"
	});

	// enable linking to diagrams
	if($(location).attr("hash") == "#inheritance-diagram") {
		diagrams.toggle($("#inheritance-diagram-container"), true);
	} else if($(location).attr("hash") == "#content-diagram") {
		diagrams.toggle($("#content-diagram-container"), true);
	}

	$(".diagram-link").click(function() {
		diagrams.toggle($(this).parent());
	});

	// register resize function
	$(window).resize(diagrams.resize);

	// don't bubble event to parent div
	// when clicking on a node of a resized
	// diagram
	$("svg a").click(function(e) {
		e.stopPropagation();
	});

	diagrams.initHighlighting();

    $("button#diagram-fs").click(function() {
        $(".diagram-container").toggleClass("full-screen");
        $(".diagram-container > div.diagram").css({
            height: $("svg").height() + "pt"
        });

        $panzoom.panzoom("reset", { animate: false, contain: false });
    });
});

/**
 * Initializes highlighting for nodes and edges.
 */
diagrams.initHighlighting = function()
{
	// helper function since $.hover doesn't work in IE

	function hover(elements, fn)
	{
		elements.mouseover(fn);
		elements.mouseout(fn);
	}

	// inheritance edges

	hover($("svg .edge.inheritance"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		var parts = $(this).attr("id").split("_");
		toggleClass($("#" + parts[0] + "_" + parts[1]));
		toggleClass($("#" + parts[0] + "_" + parts[2]));
		toggleClass($(this));
	});

	// nodes

	hover($("svg .node"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		toggleClass($(this));
		var parts = $(this).attr("id").split("_");
		var index = parts[1];
		$("svg#" + parts[0] + " .edge.inheritance").each(function(){
			var parts2 = $(this).attr("id").split("_");
			if(parts2[1] == index)
			{
				toggleClass($("#" + parts2[0] + "_" + parts2[2]));
				toggleClass($(this));
			} else if(parts2[2] == index)
			{
				toggleClass($("#" + parts2[0] + "_" + parts2[1]));
				toggleClass($(this));
			}
		});
	});

	// incoming implicits

	hover($("svg .node.implicit-incoming"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		toggleClass($(this));
		toggleClass($("svg .edge.implicit-incoming"));
		toggleClass($("svg .node.this"));
	});

	hover($("svg .edge.implicit-incoming"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		toggleClass($(this));
		toggleClass($("svg .node.this"));
		$("svg .node.implicit-incoming").each(function(){
			toggleClass($(this));
		});
	});

	// implicit outgoing nodes

	hover($("svg .node.implicit-outgoing"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		toggleClass($(this));
		toggleClass($("svg .edge.implicit-outgoing"));
		toggleClass($("svg .node.this"));
	});

	hover($("svg .edge.implicit-outgoing"), function(evt){
		var toggleClass = evt.type == "mouseout" ? diagrams.removeClass : diagrams.addClass;
		toggleClass($(this));
		toggleClass($("svg .node.this"));
		$("svg .node.implicit-outgoing").each(function(){
			toggleClass($(this));
		});
	});
};

/**
 * Resizes the diagrams according to the available width.
 */
diagrams.resize = function() {
    // available width
    var availableWidth = $(".diagram-container").width();

    $(".diagram-container").each(function() {
        // unregister click event on whole div
        $(".diagram", this).unbind("click");
        var diagramWidth = $(".diagram", this).data("width");
        var diagramHeight = $(".diagram", this).data("height");

        if (diagramWidth > availableWidth) {
            // resize diagram
            var height = diagramHeight / diagramWidth * availableWidth;
            $(".diagram svg", this).width(availableWidth);
            $(".diagram svg", this).height(height);
        } else {
            // restore full size of diagram
            $(".diagram svg", this).width(diagramWidth);
            $(".diagram svg", this).height(diagramHeight);
            // don't show custom cursor any more
            $(".diagram", this).removeClass("magnifying");
        }
    });
};

/**
 * Shows or hides a diagram depending on its current state.
 */
diagrams.toggle = function(container, dontAnimate)
{
    // change class of link
    $(".diagram-link", container).toggleClass("open");
    // get element to show / hide
    var div = $(".diagram", container);
    if (div.is(':visible')) {
        $(".diagram-help", container).hide();
        div.unbind("click");
        div.slideUp(100);

        $("#diagram-controls", container).hide();
        $("#inheritance-diagram-container").unbind('mousewheel.focal');
    } else {
        diagrams.resize();
        if(dontAnimate)
            div.show();
        else
            div.slideDown(100);
        $(".diagram-help", container).show();

        $("#diagram-controls", container).show();

        $(".diagram-container").on('mousewheel.focal', function(e) {
            e.preventDefault();
            var delta = e.delta || e.originalEvent.wheelDelta;
            var zoomOut = delta ? delta < 0 : e.originalEvent.deltaY > 0;
            $panzoom.panzoom('zoom', zoomOut, {
                increment: 0.1,
                animate: true,
                focal: e
            });
        });
    }
};

/**
 * Helper method that adds a class to a SVG element.
 */
diagrams.addClass = function(svgElem, newClass) {
	newClass = newClass || "over";
	var classes = svgElem.attr("class");
	if ($.inArray(newClass, classes.split(/\s+/)) == -1) {
		classes += (classes ? ' ' : '') + newClass;
		svgElem.attr("class", classes);
	}
};

/**
 * Helper method that removes a class from a SVG element.
 */
diagrams.removeClass = function(svgElem, oldClass) {
	oldClass = oldClass || "over";
	var classes = svgElem.attr("class");
	classes = $.grep(classes.split(/\s+/), function(n, i) { return n != oldClass; }).join(' ');
	svgElem.attr("class", classes);
};
