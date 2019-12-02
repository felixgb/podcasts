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
function getAll() {
    return __awaiter(this, void 0, void 0, function* () {
        const res = yield fetch(`http://localhost:3000/all`);
        return yield res.json();
    });
}
function nameElem(n) {
    const e = document.createElement("h1");
    e.textContent = n;
    return e;
}
function makeEpisode(p) {
    const e = document.createElement("div");
    e.classList.add("ep");
    const title = document.createElement("h2");
    title.textContent = p.title;
    e.appendChild(title);
    const desc = document.createElement("p");
    desc.appendChild(htmlToElement(p.description));
    e.appendChild(desc);
    return e;
}
function makePodcastElement(p) {
    const e = document.createElement("div");
    e.classList.add("podcast");
    const name = nameElem(p.name);
    e.appendChild(name);
    p.eps.map(makeEpisode).forEach(ep => e.appendChild(ep));
    return e;
}
function go() {
    return __awaiter(this, void 0, void 0, function* () {
        const ps = yield getAll();
        const e = document.getElementById("here");
        ps.map(makePodcastElement).forEach(p => e.appendChild(p));
    });
}
go();
function htmlToElement(html) {
    const template = document.createElement('template');
    html = html.trim();
    template.innerHTML = html;
    return template.content;
}
// function appendPodcast(p: Podcast, t: HTMLTableElement): void {
//   const row = t.insertRow(-1)
//   const nameCell = row.insertCell(0)
//   const epNumCell = row.insertCell(1)
//   const listenCell = row.insertCell(2)
//   const nextEpCell = row.insertCell(3)
//   nameCell.appendChild(document.createTextNode(p.name))
//   epNumCell.appendChild(document.createTextNode(p.epNum.toString()))
//   listenCell.appendChild(listenButton(p))
//   nextEpCell.appendChild(nextEpButton(p))
// }
// 
// function listenButton(p: Podcast): HTMLButtonElement {
//   const b = document.createElement("button") as HTMLButtonElement
//   b.innerText = "listen"
//   b.onclick = async () => {
//     const res = await fetch(`http://localhost:3000/start/${p.name}`)
//     const url = await res.text()
//     window.open(url)
//   }
//   return b
// }
// 
// function nextEpButton(p: Podcast): HTMLButtonElement {
//   const b = document.createElement("button") as HTMLButtonElement
//   b.innerText = "next ep"
//   b.onclick = async () => {
//     const res = await fetch(`http://localhost:3000/inc/${p.name}`)
//     const url = await res.text()
//     window.location.reload(true)
//   }
//   return b
// }
// 
// async function loadAll() {
//   const res = await fetch(`http://localhost:3000/all`)
//   const body = await res.json() as Array<Podcast>
//   body.forEach(p => appendPodcast(p, table))
// }
