
function gen_good_commits_table(element_id, data_url, height_string) {

	const column_list = [
		{title: "Commit", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
			    return sha1_link(cell.getValue());
			},
		},
		{title: "Age", field: "commit_timestamp", width: 100, formatter: function(cell, formatterParams, onRendered) {
			    return moment(cell.getValue()).fromNow();
			},
		},
		{title: "Unbuilt", field: "unbuilt_required_job_count", width: 125,},
		{title: "Failed", field: "failed_required_build_count", width: 125,
		},
		{title: "Disqualifying jobs", field: "disqualifying_jobs", formatter: function(cell, formatterParams, onRendered) {
			    return cell.getValue().join(", ");
			},
			tooltip: function(cell) {
				return cell.getValue().join("\n");
			},
		},
		{title: "Unbuilt or Failed", field: "not_succeeded_required_job_count", width: 200,
		},
/*
		{title: "commit_id", field: "commit_id", width: 75,
		},
*/
		{title: "Required", field: "total_required_commit_job_count", width: 125,
		},
	];

	const table = new Tabulator("#" + element_id, {
		height: height_string,
		initialSort:[
			{column: "commit_timestamp", dir: "desc"},
		],
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
	});
}


function requery_table() {
	const missing_success_threshold = $("#max-absent-successes-input").val();
	const failing_builds_threshold = $("#max-failing-builds-input").val();

	gen_good_commits_table("good-commits-table", "/api/cleanest-master-commits?missing-threshold=" + missing_success_threshold + "&failing-threshold=" + failing_builds_threshold, 300);
	return false;
}

function main() {
	requery_table();
}

