"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
console.log('hello');
const server = window.location.host || "localhost:3000";
console.log(server);
function getAll() {
    return __awaiter(this, void 0, void 0, function* () {
        const res = yield fetch(`http://${server}/all`);
        return yield res.json();
    });
}
function nameElem(n) {
    const e = document.createElement("h1");
    e.textContent = n;
    return e;
}
function listenButton(epNum, name, p) {
    const b = document.createElement("button");
    b.innerText = "this one";
    b.onclick = () => __awaiter(this, void 0, void 0, function* () {
        const listenEvent = new CustomEvent('listen', { detail: { epNum, name } });
        document.dispatchEvent(listenEvent);
        yield fetch(`http://${server}/set-ep-num/${name}/${epNum}`);
        window.open(p.url);
    });
    return b;
}
function makeEpisode(epNum, name, p) {
    const e = document.createElement("div");
    e.classList.add("ep");
    const title = document.createElement("h2");
    title.textContent = p.title;
    e.appendChild(title);
    const butt = listenButton(epNum, name, p);
    e.appendChild(butt);
    const desc = document.createElement("p");
    desc.appendChild(htmlToElement(p.description));
    e.appendChild(desc);
    return e;
}
class PodcastList {
    constructor(state) {
        this.elems = state.eps.map((ep, idx) => makeEpisode(idx, state.name, ep));
        this.selected = this.elems[state.epNum];
        this.selected.classList.add("selectedEp");
        this.elem = this.html(state);
    }
    select(n) {
        this.selected.classList.remove("selectedEp");
        this.selected = this.elems[n];
        this.selected.classList.add("selectedEp");
    }
    html(p) {
        const e = document.createElement("div");
        e.classList.add("podcast");
        const name = nameElem(p.name);
        e.appendChild(name);
        const b = document.createElement("button");
        b.innerText = "scroll to now";
        b.onclick = () => {
            this.selected.scrollIntoView();
        };
        e.appendChild(b);
        this.elems.forEach(ep => e.appendChild(ep));
        return e;
    }
}
class PodcastPage {
    constructor(state) {
        this.podcasts = new Map();
        this.onListen = (e) => {
            const list = this.podcasts.get(e.detail.name);
            if (list) {
                list.select(e.detail.epNum);
            }
        };
        state.forEach(p => this.podcasts.set(p.name, new PodcastList(p)));
        document.addEventListener('listen', this.onListen);
        this.elem = this.html();
    }
    html() {
        const e = document.createElement("div");
        e.classList.add("page");
        Array.from(this.podcasts.values()).forEach(v => e.appendChild(v.elem));
        return e;
    }
}
function go() {
    return __awaiter(this, void 0, void 0, function* () {
        const ps = yield getAll();
        const page = new PodcastPage(ps);
        document.body.appendChild(page.elem);
    });
}
go();
function htmlToElement(html) {
    const template = document.createElement('template');
    html = html.trim();
    template.innerHTML = html;
    return template.content.firstChild;
}
