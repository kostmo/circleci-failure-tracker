
function get_cause_id() {
	var urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('cause');
}


function main() {

	var cause_id = get_cause_id();

	alert("cause: " + cause_id);
}

