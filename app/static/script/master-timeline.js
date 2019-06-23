
function get_timeline_data() {

	$.getJSON('/api/master-timeline', {"count": 10}, function (mydata) {
		gen_timeline_table("master-timeline-table", mydata);
	});
}


function gen_timeline_table(element_id, fetched_data) {

	var column_list = [];

	var commit_column_definition = {
		title: "Commit",
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {
			return render_tag("code", link(cell.getValue().substring(0, 7), "/commit-details.html?sha1=" + cell.getValue()));
		},
		width: 75,
	};

	column_list.push(commit_column_definition);

	var filtered_column_names = fetched_data.columns.filter(x => !x.startsWith("binary"));
	for (var col of filtered_column_names) {
		var col_dict = {
			title: col,
			field: col,
			headerVertical: "flip",
			formatter: "tickCross",
			formatterParams: {allowEmpty: true},
		};
		column_list.push(col_dict);
	}


	var build_failures_by_commit = {};

	for (var commit_obj of fetched_data.failures) {

		var failures_by_job_name = setDefault(build_failures_by_commit, commit_obj.commit, {});
		failures_by_job_name[commit_obj.job_name] = commit_obj;
	}

	var table_data = [];
	for (var commit_obj of fetched_data.commits) {
		var row_dict = {};

		var sha1 = commit_obj.record;
		var failures_by_job_name = build_failures_by_commit[sha1] || {};

		row_dict["commit"] = sha1;


		for (var job_name in failures_by_job_name) {
			row_dict[job_name] = false;
		}

		table_data.push(row_dict);
	}


	var table = new Tabulator("#" + element_id, {
		layout: "fitColumns",
		placeholder: "No Data Set",
		columns: column_list,
		data: table_data, //set initial table data
	});
}


function main() {

	console.log("Hello");
	get_timeline_data();
}

