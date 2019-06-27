
function gen_breakages_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Action", columns: [
				{title:"X",
					headerSort: false,
					formatter: function(cell, formatterParams, onRendered){ //plain text value
					    return "<img src='/images/trash-icon.png' style='width: 16;'/>";
					},
					width:40,
					align:"center",
					cellClick:function(e, cell) {

						var cause_id = cell.getRow().getData()["start"]["db_id"];

						if (confirm("Realy delete cause #" + cause_id + "?")) {
							post_modification("/api/code-breakage-delete", {"cause_id": cause_id});
						}
					},
				},
				{title:"?",
					headerSort: false,
					formatter: function(cell, formatterParams, onRendered){ //plain text value
					    return "<img src='/images/view-icon.png' style='width: 16;'/>";
					},
					width:40,
					align:"center",
					cellClick:function(e, cell) {
						var cause_id = cell.getRow().getData()["start"]["db_id"];
						window.location.href = "/breakage-details.html?cause=" + cause_id;
					},
				},
			]},
			{title: "Description", width: 250, field: "start.record.payload.description",
				editor: "input",
				cellEdited: function(cell) {
					var cause_id = cell.getRow().getData()["start"]["db_id"];
					var new_description = cell.getValue();


					var data_dict = {"cause_id": cause_id, "description": new_description};
					post_modification("/api/code-breakage-description-update", data_dict);
				},
			},

			{title: "Affected jobs", columns: [
				{title: "Count",
					width: 75,
					formatter: function(cell, formatterParams, onRendered) {
						var joblist = cell.getRow().getData()["start"]["record"]["payload"]["affected_jobs"];
						return joblist.length;
					},
				},
				{title: "Names", field: "start.record.payload.affected_jobs",
					formatter: function(cell, formatterParams, onRendered) {
						var cell_val = cell.getValue();
						var items = [];
						for (var jobname of cell_val) {
							items.push(jobname);
						}

						return items.join(", ");
					},
				},
			]},
			{title: "Start", columns: [
				{title: "commit", width: 300, field: "start.record.payload.breakage_commit.record",
					formatter: function(cell, formatterParams, onRendered) {
						var cell_val = cell.getValue();
						return cell_val == null ? "" : sha1_link(cell_val);
					},
				},
				{title: "authorship", width: 250, field: "start.record.created",
					formatter: function(cell, formatterParams, onRendered) {
						var val = cell.getValue();
						var start_obj = cell.getRow().getData()["start"];
						return moment(val).fromNow() + " by " + start_obj["record"]["author"];;
					},
				},
			]},
			{title: "End", columns: [
				{title: "commit", width: 300, field: "end.record.payload.resolution_commit.record",
					formatter: function(cell, formatterParams, onRendered) {
						var cell_val = cell.getValue();
						return cell_val == null ? "" : sha1_link(cell_val);
					},
				},
				{title: "authorship", width: 250, field: "end.record.created",
					formatter: function(cell, formatterParams, onRendered) {
						var val = cell.getValue();

						if (val) {
							var end_obj = cell.getRow().getData()["end"];
							return moment(val).fromNow() + " by " + end_obj["record"]["author"];
						}
						return "";
					},
				},
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

