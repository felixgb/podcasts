import { haha } from "./lol.js"

console.log('hello')

type Podcast = {
  name: string,
  epNum: number
}

const table = document.getElementById("eps-body") as HTMLTableElement

function appendPodcast(p: Podcast, t: HTMLTableElement): void {
  const row = t.insertRow(-1)
  const nameCell = row.insertCell(0)
  const epNumCell = row.insertCell(1)
  const listenCell = row.insertCell(2)
  const nextEpCell = row.insertCell(3)
  nameCell.appendChild(document.createTextNode(p.name))
  epNumCell.appendChild(document.createTextNode(p.epNum.toString()))
  listenCell.appendChild(listenButton(p))
  nextEpCell.appendChild(nextEpButton(p))
}

function listenButton(p: Podcast): HTMLButtonElement {
  const b = document.createElement("button") as HTMLButtonElement
  b.innerText = "listen"
  b.onclick = async () => {
    const res = await fetch(`http://localhost:3000/start/${p.name}`)
    const url = await res.text()
    window.open(url)
  }
  return b
}

function nextEpButton(p: Podcast): HTMLButtonElement {
  const b = document.createElement("button") as HTMLButtonElement
  b.innerText = "next ep"
  b.onclick = async () => {
    const res = await fetch(`http://localhost:3000/inc/${p.name}`)
    const url = await res.text()
    window.location.reload(true)
  }
  return b
}


async function loadAll() {
  const res = await fetch(`http://localhost:3000/all`)
  const body = await res.json() as Array<Podcast>
  body.forEach(p => appendPodcast(p, table))
}

loadAll()
