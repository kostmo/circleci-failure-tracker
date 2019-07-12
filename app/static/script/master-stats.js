
function breakdown() {

	$.getJSON('/api/master-build-stats', function (data) {
		console.log(JSON.stringify(data));
	});
}

function main() {
	breakdown();
}

