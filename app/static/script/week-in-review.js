function gen_missing_required_builds_table(element_id, data_url) {

	const column_list = [
		{title: "Job", field: "job_name", width: 350},
		{title: "Count", field: "count", width: 75},
		{title: "Latest absence", field: "latest_absence_at", width: 100, formatter: function(cell, formatterParams, onRendered) {
			    return moment(cell.getValue()).fromNow();
			},
		},
	];

	const table = new Tabulator("#" + element_id, {
		height: 300,
		layout: "fitColumns",
		selectable: true,
		placeholder: "No Data Set",
		columns: column_list,
		ajaxURL: data_url,
	});

	return table;
}


function main() {

	gen_missing_required_builds_table("missing-required-jobs-table", "/api/missing-required-builds");
}
