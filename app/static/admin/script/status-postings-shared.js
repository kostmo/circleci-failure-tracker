
function gen_postings_table(element_id, data_url, height) {

	var table = new Tabulator("#" + element_id, {
		height: height,
		layout: "fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
					return '<code><a href="/commit-details.html?sha1=' + cell.getValue() + '">' + cell.getValue().substring(0, 7) + '</a></code>';
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
