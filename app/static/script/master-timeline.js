
function get_timeline_data() {

	$.getJSON('/api/master-timeline', {"count": 60}, function (mydata) {
		gen_timeline_table("master-timeline-table", mydata);
	});
}

// global
var build_failures_by_commit;

function gen_timeline_table(element_id, fetched_data) {

	var column_list = [];

	var commit_column_definition = {
		title: "Commit",
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {
			return render_tag("code", link(cell.getValue().substring(0, 7), "/commit-details.html?sha1=" + cell.getValue()));
		},
		minWidth: 90,
		width: 90,
		resizable: true,
		headerSort: false,
		frozen: true,
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
			width: 22,
			minWidth: 22,
			resizable: false,
			headerSort: false,
			cssClass: "smallish",

/*
			accessor: function(value, data, type, params, column) {
				//value - original value of the cell
				//data - the data for the row
				//type - the type of access occurring  (data|download|clipboard)
				//params - the accessorParams object passed from the column definition
				//column - column component for the column this accessor is bound to

				return true;
			},
*/

			cellClick: function(e, cell){
				//e - the click event object
				//cell - cell component
				var cell_value = cell.getValue()
				console.log("cell: " + cell_value + "; commit: " + cell.getRow().getData()["commit"]);

				if (cell_value != null) {
					console.log("build: " + cell_value["build_id"]);
				}

			},

		};
		column_list.push(col_dict);
	}

	// global
	build_failures_by_commit = {};

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
			row_dict[job_name] = failures_by_job_name[job_name];
//			row_dict[job_name] = true;
		}

		table_data.push(row_dict);
	}


	var table = new Tabulator("#" + element_id, {
		layout: "fitColumns",
		placeholder: "No Data Set",
		selectable: true,
		columns: column_list,
		data: table_data, //set initial table data
	});
}


function main() {
	get_timeline_data();
}

