function gen_missing_required_builds_table(element_id, data_url) {

	const column_list = [
		{title: "Commit", field: "sha1", width: 100,
			formatter: function(cell, formatterParams, onRendered) {
				return sha1_link(cell.getValue());
			},
		},
		{title: "Inserted at", field: "inserted_at", width: 100, formatter: function(cell, formatterParams, onRendered) {
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


function empty_queue() {
	post_modification("/api/purge-stale-work-queue-entries", {});
}


function main() {

	gen_missing_required_builds_table("queued-commits-table", "/api/scan-commits-queue");
}
