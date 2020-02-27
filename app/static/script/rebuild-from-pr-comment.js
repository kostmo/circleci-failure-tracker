function get_sha1_query_parm() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('sha1');
}


function load_rebuild_info(commit_sha1) {

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
		const reran_build_obj_list = data.content.content.reran_builds.map(x => x[0]);

		$("#rerun-count-placeholder").html(reran_build_obj_list.length);

		const builds_list = build_obj_list.map(x => render_tag("code", x["commit_build"]["build"]["build_record"]["job_name"] + "; UID: " + x["commit_build"]["build"]["universal_build"]["db_id"]));

		$("#build-list-placeholder").html(render_list(builds_list));

		$('#options-form').show();
	});
}


function main() {

	const commit_sha1 = get_sha1_query_parm();

	if (commit_sha1) {
		load_rebuild_info(commit_sha1);
	} else {
		alert("A commit SHA1 is expected as a URL query parameter!");
	}
}

