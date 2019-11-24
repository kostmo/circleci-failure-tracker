
function gen_postings_table(element_id, data_url, height) {

	var table = new Tabulator("#" + element_id, {
		height: height,
		layout: "fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				}
			},
			{title: "Message", field: "description", sorter: "string"},
			{title: "Status", field: "state", sorter: "string"},
			{title: "Time", field: "created_at", formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
		],
		ajaxURL: data_url,
	});
}






function gen_comment_postings_table(element_id, data_url, height) {

	var table = new Tabulator("#" + element_id, {
		height: height,
		layout: "fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "PR", field: "pr_number", sorter: "number",
				formatter: "link",
				formatterParams: {
//					label: "View",
					urlPrefix: PULL_REQUEST_URL_PREFIX,
				}
			},
			{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				}
			},
			{title: "Author", field: "github_user_login", sorter: "string"},
			{title: "Body", field: "body", sorter: "string",
				tooltip: function(cell) {
					return cell.getValue();
				},
			},
			{title: "Updated", field: "updated_at", formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
			{title: "Created", field: "created_at", formatter: function(cell, formatterParams, onRendered) {
					return moment(cell.getValue()).fromNow();
				}
			},
			{title: "Revisions", field: "revision_count", sorter: "number"},
		],
		ajaxURL: data_url,
	});
}



