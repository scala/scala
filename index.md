---
title: Scala Language Reference
layout: default
---

<ul>
  {% assign sorted_pages = site.pages | sort:"name" %}
  {% for post in sorted_pages %}
    <li>
      <a href="{{site.baseurl}}{{ post.url }}">{{ post.title }}</a>
    </li>
  {% endfor %}
</ul>

