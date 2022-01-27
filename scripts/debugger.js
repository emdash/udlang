"use strict";

const debug = (...x) => { console.log(x); return x[0]; };


// Monkey-Patch the DOM to make our work a little easier

function normalize(content) {
    if (content instanceof HTMLElement) {
	return content;
    } else {
	return document.createTextNode(content.toString());
    }
}

Object.prototype.map = function(f) {
    return Object.entries(this).map(([key, value]) => f(key, value));
};

Element.prototype.append = function (...content) {
    for (let c of content) {
	this.appendChild(normalize(c));
    }
    return this;
};

Element.prototype.attr = function(key, value) {
    this.setAttribute(key, value);
    return this;
};

Element.prototype.prop = function(key, value) {
    this[key] = value;
    return this;
};

Element.prototype.on = function(event, handler) {
    this.addEventListener(event, handler);
    return this;
};


// A minimal HTML Embedded DSL

const el    = tag => document.createElement(tag);
const div   = ()  => el("div");
const span  = ()  => el("span");
const ul    = ()  => el("ul");
const li    = ()  => el("li");
const table = ()  => el("table");
const tr    = ()  => el("tr");
const td    = ()  => el("td");

const datum = (k, v) => tr()
      .append(td().attr("class", "label").append(k),
	      td().attr("class", "value").append(v));

const render_value = v => { switch (v.type) {
    case "value":  return span().attr("class", "value").append(`val: ${v.value}`);
    case "app":    return span().attr("class", "app").append("app");
    case "global": return span().attr("class", "global").append(`global: ${v.name}`);
    case "ind":    return span().attr("class", "ind").append(`ind`);
}};

const render_heap = heap => table()
      .attr("class", "heap")
      .append(...heap.map((key, value) => datum(key, render_value(value))))

const render_state = state => table()
      .attr("class", "state")
      .append(datum("remark",      state.remark),
	      datum("instruction", state.instruction),
	      datum("stack",       state.stack),
	      datum("heap",        render_heap(state.heap)));

const render = () => div()
      .attr("id", "content")
      .append(...trace.map(render_state));


// Trigger the actual render

document.getElementById("content").replaceWith(render());