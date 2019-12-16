

function gen_pr_table(div_id, data) {

	const column_list = [
		{title: "PR #",
			field: "pr_number",
			formatter: "link",
			width: 75,
			formatterParams: {
				urlPrefix: "https://github.com/pytorch/pytorch/pull/"
			}
		},
		{title: "Date", field: "committer_date", 
			width: 125,
			formatter: function(cell, formatterParams, onRendered) {
				return moment(cell.getValue()).fromNow();
			},
		},
		{title: "Commits", columns: [
			{title: "PR HEAD", field: "pr_head_commit",
				formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				},
			},
			{title: "Master", field: "master_commit",
				formatter: function(cell, formatterParams, onRendered) {
					return sha1_link(cell.getValue());
				},
			},
		]},
		{title: "Builds", columns: [
			{title: "Green", field: "all_succeeded", formatter:"tickCross", sorter:"boolean", width: 75},
			{title: "Total", field: "total_builds", width: 90},
			{title: "Succeeded", field: "succeeded_count", width: 90},
			{title: "Failed", field: "failed_count", width: 90},
			{title: "Foreshadowed breakage", field: "foreshadowed_breakage_count", width: 150},
		]},
	];

	var table = new Tabulator("#" + div_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		rowFormatter: function(row) {

			const data = row.getData();

			const had_failed_build = data["succeeded_count"] != data["total_builds"];
			const row_color = had_failed_build ? "#f002" : "#0f02";
			row.getElement().style.backgroundColor = row_color;
		},
		columns: column_list,
		data: data,
	});
}


function request_pull_requests(pr_numbers) {
	getJsonWithThrobber("#throbber", "/api/pr-batch-list", {"pr-numbers-delimited": pr_numbers.join(",")}, function (data) {
		gen_pr_table("container-pull-requests", data);
	});
}

function main() {

	const urlParams = new URLSearchParams(window.location.search);
	const pr_numbers = urlParams.getAll('pr');

	request_pull_requests(pr_numbers);
}


