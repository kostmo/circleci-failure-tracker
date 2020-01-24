
function delete_callback() {

	const cause_id = get_cause_id();

	if (confirm("Realy delete cause #" + cause_id + "?")) {
		post_modification("/api/code-breakage-delete", {"cause_id": cause_id});
	}
}


function add_affected_job() {

	const cause_id = get_cause_id();
	const new_affected_job = $("#new-affected-job-field").val();

	if (new_affected_job && confirm("Add new affected job \"" + new_affected_job + "\"?")) {
		post_modification("/api/code-breakage-add-affected-job", {"cause_id": cause_id, "job_name": new_affected_job});
	}
}


function delete_resolution() {

	const cause_id = get_cause_id();

	if (confirm("Remove resolution?")) {
		post_modification("/api/code-breakage-delete-resolution", {"cause_id": cause_id});
	}
}


function update_resolution_commit() {

	const cause_id = get_cause_id();
	const new_resolution_commit = $("#new-resolution-sha1-field").val();

	if (new_resolution_commit && confirm("Update resolution sha1 to " + new_resolution_commit + "?")) {
		post_modification("/api/code-breakage-update-resolution-sha1", {
			"cause_id": cause_id,
			"resolution_sha1": new_resolution_commit,
		});
	}
}


function update_cause_commit() {

	const cause_id = get_cause_id();
	const new_cause_commit = $("#new-cause-sha1-field").val();

	if (new_cause_commit && confirm("Update cause sha1 to " + new_cause_commit + "?")) {
		post_modification("/api/code-breakage-update-cause-sha1", {
			"cause_id": cause_id,
			"cause_sha1": new_cause_commit,
		});
	}
}


function gen_affected_jobs_table(element_id, cause_id) {

	const data_url = "/api/known-breakage-affected-jobs?cause_id=" + cause_id;

	const table = new Tabulator("#" + element_id, {
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns: [
			{title:"X",
				headerSort: false,
				formatter: function(cell, formatterParams, onRendered){
				    return "<img src='/images/trash-icon.png' style='width: 16;'/>";
				},
				width: 40,
				align: "center",
				cellClick: function(e, cell) {

					const job_name = cell.getRow().getData()["payload"];
					if (confirm("Realy delete cause #" + cause_id + "?")) {
						post_modification("/api/code-breakage-job-delete", {
							"cause_id": cause_id,
							"job": job_name,
						});
					}
				},
			},
			{title: "Job", field: "payload.job_name", sorter: "string"},
			{title: "Success rate", field: "payload.succeeded_count", "width": 150,
				formatter: function(cell, formatterParams, onRendered) {
					const val = cell.getValue();

					const row_data = cell.getRow().getData();
					const built_count = row_data["payload"]["total_built_count"];
					return val + " / " + built_count + " (" + (val*100.0/built_count).toFixed(1) + "%)";
				},
			},
			{title: "Reported", width: 250, field: "created",
				formatter: function(cell, formatterParams, onRendered) {
					const val = cell.getValue();

					const row_data = cell.getRow().getData();
					return moment(val).fromNow() + " by " + render_tag("b", row_data["author"]);
				},
			},
		],
		ajaxURL: data_url,
	});
}


function get_cause_id() {
	const urlParams = new URLSearchParams(window.location.search);
	return urlParams.get('cause');
}


function main() {

	const cause_id = get_cause_id();

	/* TODO Too slow
	$("#scan-throbber").show();
	$.getJSON('/api/list-failure-modes', function (mydata) {

		$("#scan-throbber").hide();

		const failure_modes_dict = {};
		for (var item of mydata) {
			failure_modes_dict[item["db_id"]] = item["record"];
		}

		gen_annotated_breakages_table("annotated-breakages-table", "/api/code-breakages-annotated-single?cause_id=" + cause_id, failure_modes_dict);
	});
	*/


// TODO: use "/api/code-breakage-mode-single" API

	const jquery_selector_element = setup_breakage_mode_selector();

	$.getJSON('/api/code-breakage-mode-single', {"cause_id": cause_id}, function (mydata) {

		console.log("mydata: " + JSON.stringify(mydata))
		jquery_selector_element.val(mydata);
	});

	gen_affected_jobs_table("affected-jobs-table", cause_id);

	$("#button-breakage-mode-update").click(function() {
		const new_failure_mode = jquery_selector_element.val();
		update_breakage_mode(cause_id, new_failure_mode);
	});
}

