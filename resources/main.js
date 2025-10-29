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
			link.style = 'color: var(--foreground2-color)'
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

function checkRes ( { success, message } ) {
	let ret = true;

	if (success) {
		showMessage ({title: "Success", type: "success", message})
		setTimeout (() => window.location.reload(), 2000);
	} else {
		showMessage ({title: "Error", type: "error", message})
		ret = false;
	}

	return ret
}

function newCategory ( query ) {
	return fetch (`/new?type=${query[1]}&title=${query[2]}&color=${query[3]}`).then ( ( res ) => res.json () )
}

function newItem ( query ) {
	return fetch (`/new?type=${query[1]}&title=${query[2]}&category=${query[3]}&url=${query[4]}`).then ( ( res ) => res.json () )
}

function deleteReq ( query ) {
	let selector = ( query[1] == "item" ) ? ".category-item" : ".category-name"
	let elems = [...document.querySelectorAll ( selector )].map ( ( i ) => i.innerHTML );
	let res;

	if (elems.includes (query [2])) {
		res = fetch (`/delete?type=${query[1]}&title=${query[2]}`).then ( ( res ) => res.json () );
	} else {
		res = new Promise ((resolve, reject) => {resolve ({success: false, message: "No such item or cateogry"})})
	}

	return res;
}

function selectTheme ( query ) {
	return fetch (`/theme?title=${query[1]}`).then ( ( res ) => res.json () );
}

let validFuncs = {
	"new": {
		"item":		(query) => { return newItem		( query ).then ( ( res ) => checkRes ( res ) ) },
		"category": (query) => { return newCategory	( query ).then ( ( res ) => checkRes ( res ) )	}
	},
	"delete": {
		"item":		(query) => { return deleteReq	( query ).then ( ( res ) => checkRes ( res ) ) },
		"category": (query) => { return deleteReq	( query ).then ( ( res ) => checkRes ( res ) ) },
	},
	"t": (query) => { return selectTheme ( query ).then ( ( res ) => checkRes ( res ) ) },
	"s": (query) => { window.location.href = `https://google.com/search?q=${query.slice(1).join(" ")}`; return true },
	"o": (query) => { window.location.href = `https://${query.slice(1).join(" ")}`; return true },
}

let wordsCount = ["first", "second", "third"]

function validFindFunc (query, obj, wc) {
	let node = obj [ query.shift () ];
	wc += 1;

	if (typeof node == "object") {
		node = validFindFunc (query, node, wc);
	} else if (typeof node == "undefined") {
		node = () => {
			showMessage ({title: "Error", type: "error", message: `You've an error in ${wordsCount[wc - 1]} word`})
			return false;
		}
	}

	return node;
}

function validQuery (query) {
	candidates.forEach ( ( c ) => { validFuncs [c.innerHTML] = () => { window.location.href = c.href } });
	return (validFindFunc (query.split (" "), validFuncs, 0)) (query.split (" "))
}

inputField.addEventListener ('keydown', (event) => {
	if (event.key == 'Enter') {

		if ( validQuery ( event.target.value ) ) {
			event.target.value = ''
			event.target.dispatchEvent(new CustomEvent ('input'))
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
		candidates[selectedCandidate++].style = 'color: var(--accent-color)'
	}
})

changeTime ()
setInterval (changeTime, 1000)
