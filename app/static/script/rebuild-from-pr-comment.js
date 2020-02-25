
function get_sha1_query_parm() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('sha1');
}


function load_rebuild_info() {

	const commit_sha1 = get_sha1_query_parm();
	$("#commit-placeholder").html(sha1_link(commit_sha1));

	const query_parms = {
		"login_redirect_path": get_url_path_for_redirect(),
		"sha1": commit_sha1,
	};


	$("#throbber").show();
	post_inquiry("/api/rebuild-flaky-candidates", query_parms, function(data) {
		$("#throbber").hide();


		$("#username-container").html(data.user);

		console.log("data:", data);
		const build_obj_list = data.content.content;

		const builds_list = build_obj_list.map(x => render_tag("code", x["commit_build"]["build"]["build_record"]["job_name"] + "; UID: " + x["commit_build"]["build"]["universal_build"]["db_id"]));

		$("#build-list-placeholder").html(render_list(builds_list));

		$('#options-form').show();
	});


/*
	getJsonWithThrobber("#throbber",
		"/api/rebuild-flaky-candidates",
		query_parms,
		function (data) {

			if (data["success"]) {

				$("#username-container").html(data.payload.user);

				console.log("data.payload:", data.payload);
				const build_obj_list = data.payload.content.content;

				const builds_list = build_obj_list.map(x => render_tag("code", x["commit_build"]["build"]["build_record"]["job_name"] + "; UID: " + x["commit_build"]["build"]["universal_build"]["db_id"]));

				$("#build-list-placeholder").html(render_list(builds_list));

				$('#options-form').show();

			} else {

				// TODO consolidate this error handling with "handle_submission_response()" from "html-utils.js"
				if (data.error.details.authentication_failed) {
					window.location.href = data.error.details.login_url;
				}
			}
		}
	);
*/
}

/*
function submit_rebuild_request() {

	$("#submit-button").prop("disabled", true);
	$('#mini-throbber').show();

	post_modification("/api/update-user-opt-out-settings", {"enabled": $('#checkbox-comment-posting-enabled').is(":checked")});

	return false;
}
*/


function main() {

	load_rebuild_info();
}

