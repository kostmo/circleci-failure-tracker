
function get_job_name() {
	var urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('job');
}


function main() {

	var job_name = get_job_name();

	alert("Job: " + job_name);
}

