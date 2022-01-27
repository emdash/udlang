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
const img   = ()  => el("img");
const pre   = ()  => el("pre");

const datum = (k, v) => tr()
      .append(td().attr("class", "datum-label").append(k),
	      td().attr("class", "datum-value").append(v));

const render_value = v => { switch (v.type) {
    case "value":  return span().attr("class", "value").append(`val: ${v.value}`);
    case "app":    return span().attr("class", "app").append("app");
    case "global": return span().attr("class", "global").append(`global: ${v.name}`);
    case "ind":    return span().attr("class", "ind").append(`ind`);
}};

const render_heap = heap => img()
      .attr("class", "heap")
      .attr("src", heap.img);

const render_instruction = i => span().attr("class", "ins").append(i);

const render_queue = q => span()
      .append(...q.map(render_instruction));

const render_stack = (s, h) => span()
      .append(...s.map(addr => render_value(h[addr])))

const render_exception = exc => pre()
      .attr("class", "exception")
      .append(exc || "none");

const render_state = state => table()
      .attr("class", "state")
      .append(datum("remark",      state.remark),
	      datum("instruction", render_instruction(state.instruction)),
              datum("queue",       render_queue(state.queue)),
	      datum("stack",       render_stack(state.stack, state.heap.data)),
	      datum("heap",        render_heap(state.heap)),
              datum("exception",   render_exception(state.exception)));

const render = () => div()
      .attr("id", "content")
      .append(...dump.map(render_state));


// Trigger the actual render

document.getElementById("content").replaceWith(render());
