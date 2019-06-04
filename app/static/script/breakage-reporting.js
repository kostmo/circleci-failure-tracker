
function populate_breakage_form(submission_button_container_id, build_step_id, data) {

	var submission_button_html = "<button onclick='submit_breakage_report(this, \"" + build_step_id + "\");'>Update broken status</button>";
        $("#" + submission_button_container_id).html(submission_button_html);


	if (_.has(data, "breakage") && data["breakage"] != null) {

		$('#input-notes').val( data["breakage"]["notes"] );
		$('#input-implicated-revision').val( data["breakage"]["implicated_revision"] );

//			$('#input-implicated-revision').val( data["breakage"]["reporter"] );
		$('#is-broken-checkbox').prop('checked', data["breakage"]["is_broken"]);
	}
}
