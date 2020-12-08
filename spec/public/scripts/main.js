function currentChapter() {
  var path = document.location.pathname;
  var idx  = path.lastIndexOf("/") + 1;
  var chap = path.substring(idx, idx + 2);
  return parseInt(chap, 10);
}

function heading(i, heading, $heading) {
  var currentLevel = parseInt(heading.tagName.substring(1));
  var result = "";
  if (currentLevel === this.headerLevel) {
    this.headerCounts[this.headerLevel] += 1;
    return "" + this.headerCounts[this.headerLevel] + " " + $heading.text();
  } else if (currentLevel < this.headerLevel) {
    while(currentLevel < this.headerLevel) {
      this.headerCounts[this.headerLevel] = 1;
      this.headerLevel -= 1;
    }
    this.headerCounts[this.headerLevel] += 1;
    return "" + this.headerCounts[this.headerLevel]+ " " + $heading.text();
  } else {
    while(currentLevel > this.headerLevel) {
      this.headerLevel += 1;
      this.headerCounts[this.headerLevel] = 1;
    }
    return "" + this.headerCounts[this.headerLevel]+ " " + $heading.text();
  }
}

// ignore when using wkhtmltopdf, or it won't work...
if(window.jekyllEnv !== 'spec-pdf') {
  $('#toc').toc(
    {
      'selectors': 'h1,h2,h3',
      'smoothScrolling': false,
      'chapter': currentChapter(),
      'headerLevel': 1,
      'headerCounts': [-1, currentChapter() - 1, 1, 1],
      'headerText': heading
    }
  );
}

// no language auto-detect so that EBNF isn't detected as scala
hljs.configure({
  languages: []
});

// KaTeX configuration
document.addEventListener("DOMContentLoaded", function() {
  renderMathInElement(document.body, {
    delimiters: [
      {left: "´", right: "´", display: false}, // "display: false" -> inline
      {left: "$$", right: "$$", display: true}
    ],
    ignoredTags: ['script', 'noscript', 'style', 'textarea'],
  });
  // syntax highlighting after KaTeX is loaded,
  // so that math can be used in code blocks
  hljs.initHighlighting();
  $("pre nobr").addClass("fixws");
  // point when all necessary js is done, so PDF to be rendered
  window.status = "loaded";
});

$("#chapters a").each(function (index) {
 if (document.location.pathname.endsWith($(this).attr("href")))
   $(this).addClass("chapter-active");
 else
   $(this).removeClass("chapter-active");
});
