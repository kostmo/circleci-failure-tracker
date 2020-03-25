

function gen_comment_postings_table(element_id, data_url, height) {

	var table = new Tabulator("#" + element_id, {
		height: height,
		layout: "fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "PR", field: "pr_number", sorter: "number", width: 60,
				formatter: function(cell, formatterParams, onRendered) {

					const row_data = cell.getRow().getData();
					return link(cell.getValue(), PULL_REQUEST_URL_PREFIX + cell.getValue() + "#issuecomment-" + row_data["comment_id"]);
				},
			},
			{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				}
			},
			{title: "Author", field: "github_user_login", sorter: "string", width: 120},
			{title: "Body", field: "body", sorter: "string",
				headerFilter: "input",
				tooltip: function(cell) {
					return cell.getValue();
				},
			},
			{title: "Updated", field: "updated_at", width: 150, formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
/*
			{title: "Created", field: "created_at", width: 150, formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
*/



			{title: "Q time", field: "queue_residency_duration", sorter: "number", width: 90, formatter: function(cell, formatterParams, onRendered) {
					return cell.getValue().toFixed(1) + "s";
				}
			},

			{title: "Exec time", field: "execution_duration", sorter: "number", width: 90, formatter: function(cell, formatterParams, onRendered) {
					return cell.getValue().toFixed(1) + "s";
				}
			},



			{title: "Repush", field: "was_new_push", formatter:"tickCross", sorter:"boolean", width: 100},
			{title: "No fault", field: "all_no_fault_failures", formatter:"tickCross", sorter:"boolean", width: 100},
			{title: "All green", field: "all_successful_circleci_builds", formatter:"tickCross", sorter:"boolean", width: 100},
			{title: "Revs", field: "revision_count", sorter: "number", width: 125},
			{title: "Size", field: "body", sorter: "number", width: 90, formatter: function(cell, formatterParams, onRendered) {
					return cell.getValue().length;
				}
			},
		],
		ajaxURL: data_url,
	});
}



