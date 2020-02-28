

function gen_comment_postings_table(element_id, data_url, height) {

	var table = new Tabulator("#" + element_id, {
		height: height,
		layout: "fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "PR", field: "pr_number", sorter: "number", width: 80,
				formatter: function(cell, formatterParams, onRendered) {

					const row_data = cell.getRow().getData();
					return link(cell.getValue(), PULL_REQUEST_URL_PREFIX + cell.getValue() + "#issuecomment-" + row_data["comment_id"]);
				},
			},
			{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				}
			},
			{title: "Author", field: "github_user_login", sorter: "string", width: 150},
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
			{title: "Created", field: "created_at", width: 150, formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
			{title: "Revisions", field: "revision_count", sorter: "number", width: 125},
			{title: "Length", field: "body", sorter: "number", width: 90, formatter: function(cell, formatterParams, onRendered) {
					return cell.getValue().length;
				}
			},
		],
		ajaxURL: data_url,
	});
}



