
function populate_breakage_form(submission_button_container_id, git_sha1, data) {

	var submission_button_html = "<button onclick='submit_breakage_report(this, \"" + git_sha1 + "\");'>Update broken status</button>";
        $("#" + submission_button_container_id).html(submission_button_html);


	if (_.has(data, "breakage") && data["breakage"] != null) {

		$('#input-notes').val( data["breakage"]["notes"] );
		$('#input-implicated-revision').val( data["breakage"]["implicated_revision"] );

//			$('#input-implicated-revision').val( data["breakage"]["reporter"] );
		$('#is-broken-checkbox').prop('checked', data["breakage"]["is_broken"]);
	}
}
