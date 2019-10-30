
function load_opt_out_status() {

	getJsonWithThrobber("#throbber", "/api/user-opt-out-settings", {"login_redirect_path": "/hello-world"}, function (data) {

		console.log("hi", data);

	});
}


function main() {

	load_opt_out_status();
}

