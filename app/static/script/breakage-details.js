
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



function update_resolution_commit() {

	const cause_id = get_cause_id();
	const new_resolution_commit = $("#new-resolution-sha1-field").val();

	if (new_resolution_commit && confirm("Update resolution sha1 to " + new_resolution_commit + "?")) {
		post_modification("/api/code-breakage-update-resolution-sha1", {"cause_id": cause_id, "resolution_sha1": new_resolution_commit});
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
						post_modification("/api/code-breakage-job-delete", {"cause_id": cause_id, "job": job_name});
					}
				},
			},
			{title: "Job", field: "payload", sorter: "string"},
			{title: "reported", width: 250, field: "created",
				formatter: function(cell, formatterParams, onRendered) {
					const val = cell.getValue();

					const row_data = cell.getRow().getData();
					return moment(val).fromNow() + " by " + row_data["author"];
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
	gen_affected_jobs_table("affected-jobs-table", cause_id);
}

