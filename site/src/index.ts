console.log('hello')

const server = window.location.host || "localhost:3000"
console.log(server)

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

type ListenEvent = {
  name: string,
  epNum: number
}

async function getAll(): Promise<Array<PodcastsState>> {
  const res = await fetch(`http://${server}/api/all`)
  return await res.json() as Array<PodcastsState>
}

function nameElem(n: string): HTMLElement {
  const e = document.createElement("h1")
  e.textContent = n
  return e
}

function listenButton(epNum: number, name: String, p: Podcast): HTMLElement {
  const b = document.createElement("button") as HTMLButtonElement
  b.innerText = "this one"

  b.onclick = async () => {
    const listenEvent = new CustomEvent('listen', { detail: { epNum, name }})
    document.dispatchEvent(listenEvent)
    await fetch(`http://${server}/api/set-ep-num/${name}/${epNum}`)
    window.open(p.url)
  }
  return b
}

function makeEpisode(epNum: number, name: String, p: Podcast): HTMLElement {
  const e = document.createElement("div") as HTMLDivElement
  e.classList.add("ep")

  const title = document.createElement("h2")
  title.textContent = p.title
  e.appendChild(title)

  const butt = listenButton(epNum, name, p)
  e.appendChild(butt)

  const desc = document.createElement("p")
  const descText = htmlToElement(p.description)
  if (descText !== null) {
    desc.appendChild(descText)
  }
  e.appendChild(desc)

  return e
}

class PodcastList {

  elem: HTMLElement
  elems: Array<HTMLElement>
  selected: HTMLElement

  constructor(state: PodcastsState) {
    this.elems = state.eps.map((ep, idx) => makeEpisode(idx, state.name, ep))
    this.selected = this.elems[state.epNum]
    this.selected.classList.add("selectedEp")

    this.elem = this.html(state)
  }

  select(n: number) {
    this.selected.classList.remove("selectedEp")
    this.selected = this.elems[n]
    this.selected.classList.add("selectedEp")
  }

  html(p: PodcastsState): HTMLElement {
    const e = document.createElement("div") as HTMLDivElement
    e.classList.add("podcast")

    const name = nameElem(p.name)
    e.appendChild(name)

    const b = document.createElement("button") as HTMLButtonElement
    b.innerText = "scroll to now"

    b.onclick = () => {
      this.selected.scrollIntoView()
    }
    e.appendChild(b)

    this.elems.forEach(ep => e.appendChild(ep))

    return e
  }

}

class PodcastPage {
  
  podcasts: Map<string, PodcastList> = new Map()
  elem: HTMLElement

  constructor(state: Array<PodcastsState>) {
    state.forEach(p => this.podcasts.set(p.name, new PodcastList(p)))
    document.addEventListener('listen', this.onListen as EventListener)
    this.elem = this.html()
  }

  onListen = (e: CustomEvent<ListenEvent>) => {
    const list = this.podcasts.get(e.detail.name)
    if (list) {
      list.select(e.detail.epNum)
    }
  }

  html(): HTMLElement {
    const e = document.createElement("div") as HTMLDivElement
    e.classList.add("page")
    Array.from(this.podcasts.values()).forEach(v => e.appendChild(v.elem))
    return e
  }

}

async function go(): Promise<void> {
  const ps = await getAll()
  const page = new PodcastPage(ps)
  document.body.appendChild(page.elem)
}

go()

function htmlToElement(html: string): HTMLElement {
  const template = document.createElement('template')
  html = html.trim()
  template.innerHTML = html
  console.log(template.content)
  console.log(template.content.firstChild)
  return template.content.firstChild as HTMLElement
}
// 
// function stripClasses(e: HTMLElement) {
//   e.className = ''
//   console.log(e)
//   Array.from(e.children).forEach(p => {
//     console.log(p)
//     stripClasses(p)
//   })
// }
