
function gen_breakages_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Description", width: 250, field: "start.record.payload.description",
				formatter: function(cell, formatterParams, onRendered) {
					var cell_val = cell.getValue();
					var cause_id = cell.getRow().getData()["start"]["db_id"];
					return link(cell_val, "/breakage-details.html?cause=" + cause_id);
				},
			},
			{title: "Affected jobs", width: 250, field: "start.record.payload.affected_jobs",
				formatter: function(cell, formatterParams, onRendered) {
					var cell_val = cell.getValue();
					var items = [];
					for (var jobname of cell_val) {
						items.push(jobname);
					}
					return items.join(", ");
				},
			},
			{title: "Start", columns: [
				{title: "commit", width: 300, field: "start.record.payload.breakage_commit.record",
					formatter: function(cell, formatterParams, onRendered) {
						var cell_val = cell.getValue();
						return cell_val == null ? "" : sha1_link(cell_val);
					},
				},
				{title: "authorship", width: 250, field: "start.record.created"},
			]},
			{title: "End", columns: [
				{title: "commit", width: 300, field: "end.record.payload.resolution_commit.record",
					formatter: function(cell, formatterParams, onRendered) {
						var cell_val = cell.getValue();
						return cell_val == null ? "" : sha1_link(cell_val);
					},
				},
				{title: "authorship", width: 250, field: "end.record.created"},
			]},

		],
		ajaxURL: data_url,
/*		ajaxResponse: function(url, params, response) {
			return response.payload;
		},
*/
	});
}


function main() {

	gen_breakages_table("code-breakages-table", "/api/code-breakages");
}

