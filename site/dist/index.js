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
const table = document.getElementById("eps-body");
function appendPodcast(p, t) {
    const row = t.insertRow(-1);
    const nameCell = row.insertCell(0);
    const epNumCell = row.insertCell(1);
    const listenCell = row.insertCell(2);
    const nextEpCell = row.insertCell(3);
    nameCell.appendChild(document.createTextNode(p.name));
    epNumCell.appendChild(document.createTextNode(p.epNum.toString()));
    listenCell.appendChild(listenButton(p));
    nextEpCell.appendChild(nextEpButton(p));
}
function listenButton(p) {
    const b = document.createElement("button");
    b.innerText = "listen";
    b.onclick = () => __awaiter(this, void 0, void 0, function* () {
        const res = yield fetch(`http://localhost:3000/start/${p.name}`);
        const url = yield res.text();
        window.open(url);
    });
    return b;
}
function nextEpButton(p) {
    const b = document.createElement("button");
    b.innerText = "next ep";
    b.onclick = () => __awaiter(this, void 0, void 0, function* () {
        const res = yield fetch(`http://localhost:3000/inc/${p.name}`);
        const url = yield res.text();
        window.location.reload(true);
    });
    return b;
}
function loadAll() {
    return __awaiter(this, void 0, void 0, function* () {
        const res = yield fetch(`http://localhost:3000/all`);
        const body = yield res.json();
        body.forEach(p => appendPodcast(p, table));
    });
}
loadAll();
