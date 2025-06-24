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

function validQuery (query) {
	let ret = true;
	let allowedWords = [["new", "delete"], ["item", "category"]]
	candidates.forEach ( ( i ) => allowedWords[0].push ( i.innerHTML ) )

	for (let i = 0; i < allowedWords.length; i++){
		if (query[i] && !allowedWords[i].includes (query [i]))
			ret = false;
	}

	if (query [0] == "s" || query [0] == "o") {
		ret = true;
	}

	return ret;
}

function checkRes (res, message) {
	if (res.success) {
		showMessage ({title: "Success", type: "success", message})
		setTimeout (() => window.location.reload(), 2000);
	} else {
		showMessage ({title: "Error", type: "error", message: "Some server error"})
	}
}

function newCategory ( query ) {
	return fetch (`/new?type=${query[1]}&title=${query[2]}&color=${query[3]}`).then ( ( res ) => res.json () )
}

function newItem ( query ) {
	return fetch (`/new?type=${query[1]}&title=${query[2]}&category=${query[3]}&url=${query[4]}`).then ( ( res ) => res.json () )
}

function deleteReq ( query ) {
	return fetch (`/delete?type=${query[1]}&title=${query[2]}`).then ( ( res ) => res.json () );
}

let endpoints = {
	"new": {
		"category": ( query ) => { newCategory  ( query )	.then ( ( res ) => checkRes ( res, "Category successfully added<br/>Page will reload in 2 sec" ) ) },
		"item": 	( query ) => { newItem		( query )	.then ( ( res ) => checkRes ( res, "Item successfully added<br/>Page will reload in 2 sec" ) ) },
	},
	"delete": {
		"category": ( query ) => { deleteReq	( query )	.then ( ( res ) => checkRes ( res, "Category successfully deleted<br/>Page will reload in 2 sec" ) ) },
		"item": 	( query ) => { deleteReq	( query )	.then ( ( res ) => checkRes ( res, "Item successfully deleted<br/>Page will reload in 2 sec" ) ) },
	}
}

inputField.addEventListener ('keydown', (event) => {
	if (event.key == 'Enter') {
		let splitedQuery = event.target.value.split(" ")

		if ( validQuery ( splitedQuery ) ) {

			if ( candidates.map ((i) => i.innerHTML).includes ( event.target.value ) ) {
				window.location.href = candidates[selectedCandidate - 1].href
			} else if (splitedQuery[0] == 's') {
				window.location.href = `https://google.com/search?q=${splitedQuery.slice(1).join(" ")}`
			} else if (splitedQuery[0] == 'o') {
				window.location.href = `https://${splitedQuery.slice(1).join(" ")}`
			} else {
				endpoints[splitedQuery[0]][splitedQuery[1]] (splitedQuery);
			}

			event.target.value = ''
			event.target.dispatchEvent(new CustomEvent ('input'))

		} else {
			showMessage ({title: "Error", type: "error", message: "You have some error in your query"})
		}

		candidates = [...links]
	}

	// Tab-key pressed
	if (event.keyCode == 9) {
		event.preventDefault ()

		if (selectedCandidate == candidates.length) {
			selectedCandidate = 0
		}

		// FIXME: Simplify logic
		candidates[prevCandidate].style = ''
		prevCandidate = selectedCandidate
		event.target.value = candidates[selectedCandidate].innerHTML
		candidates[selectedCandidate++].style = 'color: #BD93F9'
	}
})

changeTime ()
setInterval (changeTime, 1000)
