let dateElem	= document.getElementById ('date')
let timeElem	= document.getElementById ('time')
let inputField	= document.getElementById ('input')
let links		= document.querySelectorAll ('.category-item')
let candidates	= [...links]
let prevCandidate = 0
let selectedCandidate = 0

function changeTime () {
	let date = new Date ()

	let day		= new Intl.DateTimeFormat('en-US', {day: '2-digit'}).format(date)
	let month	= new Intl.DateTimeFormat('en-US', {month: 'long'}).format(date)
	let year	= new Intl.DateTimeFormat('en-US', {year: 'numeric'}).format(date)
	let time	= new Intl.DateTimeFormat('en-US', {hour12: true, hour: '2-digit', minute: 'numeric', second: 'numeric'}).format(date)

	dateElem.innerHTML = `${day} ${month} ${year}`
	timeElem.innerHTML = `${time}`
}

inputField.addEventListener('input', (event) => {
	candidates = []
	selectedCandidate = 0
	prevCandidate = 0

	links.forEach ((link) => {
		let linkName = link.innerHTML.toLowerCase ()
		let userInput = event.target.value.toLowerCase ()

		if ( !linkName.includes( userInput ) && userInput != '' ) {
			link.style = 'color: #6272A4'
		} else {
			link.style = ''
			candidates.push (link)
		}
	})
})

let messageCounter = 0;
function showMessage ({title, type, message}) {
	let infoContainer = document.getElementById ("info-container")
	infoContainer.insertAdjacentHTML("beforeend", `
					<div id="message-${messageCounter}" class="message message-normal container-shadow" style="opacity: 0">
						<h1 class="message-${type}">${title}</h1>
						<p>${message}<p/>
					</div>
`)

	let messageDiv = document.getElementById (`message-${messageCounter++}`)

	setTimeout (() => {
		messageDiv.style.opacity = "1"
	}, 100)

	setTimeout (() => {
		messageDiv.style.opacity = "0"
		setTimeout (() => {messageDiv.remove()}, 1000)
	}, 10000);
}

function getNewCategoryFetchString (query) {
	return `/new?type=${query[1]}&title=${query[2]}&color=${query[3]}`;
}

function getNewItemFetchString (query) {
	return `/new?type=${query[1]}&title=${query[2]}&category=${query[3]}&url=${query[4]}`
}

let newEndpoints = {
	"category": getNewCategoryFetchString,
	"item": getNewItemFetchString
}

inputField.addEventListener ('keydown', (event) => {
	if (event.key == 'Enter') {
		let splitedQuery = event.target.value.split(" ")

		if (candidates[selectedCandidate - 1]) {
			window.location.href = candidates[selectedCandidate - 1].href
		} else if (splitedQuery[0] == 's') {
			window.location.href = `https://google.com/search?q=${splitedQuery.slice(1).join(" ")}`
		} else if (splitedQuery[0] == 'new') {
			let result = fetch (newEndpoints[splitedQuery[1]](splitedQuery))
				.then((res) => res.json())
				.then((json) => {
					if (json.success) {
						showMessage ({title: "Success", type: "success", message: "Successfully added<br/>page will reload in 2 seconds"})
						setTimeout (() => window.location.reload(), 2000);
					} else {
						showMessage ({title: "Error", type: "error", message: "Some server error"})
					}
				})
		} else if (splitedQuery[0] == 'delete') {
			let result = fetch (`/delete?type=${splitedQuery[1]}&title=${splitedQuery[2]}`)
				.then((res) => res.json())
				.then((json) => {
					if (json.success) {
						showMessage ({title: "Success", type: "success", message: "Successfully deleted<br/>page will reload in 2 seconds"})
						setTimeout (() => window.location.reload(), 2000);
					} else {
						showMessage ({title: "Error", type: "error", message: "Some server error"})
					}
				})
		}

		event.target.value = ''
		candidates = [...links]
		event.target.dispatchEvent(new CustomEvent ('input'))
	}

	if (event.keyCode == 9) {
		event.preventDefault ()

		if (selectedCandidate == candidates.length) {
			selectedCandidate = 0
		}

		candidates[prevCandidate].style = ''
		prevCandidate = selectedCandidate
		event.target.value = candidates[selectedCandidate].innerHTML
		candidates[selectedCandidate++].style = 'color: #BD93F9'
	}
})

changeTime ()
setInterval (changeTime, 1000)
