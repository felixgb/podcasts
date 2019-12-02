import { haha } from "./lol.js"

console.log('hello')

type Podcast = {
  title: string,
  url: string,
  description: string
}

type PodcastsState = {
  eps: Array<Podcast>,
  name: string,
  epNum: number
}

async function getAll(): Promise<Array<PodcastsState>> {
  const res = await fetch(`http://localhost:3000/all`)
  return await res.json() as Array<PodcastsState>
}

function nameElem(n: string): HTMLElement {
  const e = document.createElement("h1")
  e.textContent = n
  return e
}

function makeEpisode(p: Podcast): HTMLElement {
  const e = document.createElement("div") as HTMLDivElement
  e.classList.add("ep")

  const title = document.createElement("h2")
  title.textContent = p.title
  e.appendChild(title)

  const desc = document.createElement("p")
  desc.appendChild(htmlToElement(p.description))
  e.appendChild(desc)

  return e
}

function makePodcastElement(p: PodcastsState): HTMLElement {
  const e = document.createElement("div") as HTMLDivElement
  e.classList.add("podcast")

  const name = nameElem(p.name)
  e.appendChild(name)

  p.eps.map(makeEpisode).forEach(ep => e.appendChild(ep))

  return e
}

async function go(): Promise<void> {
  const ps = await getAll()
  const e = document.getElementById("here") as HTMLDivElement
  ps.map(makePodcastElement).forEach(p => e.appendChild(p))
}

go()

function htmlToElement(html: string): HTMLElement {
  const template = document.createElement('template')
  html = html.trim()
  template.innerHTML = html
  return template.content as HTMLElement
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
