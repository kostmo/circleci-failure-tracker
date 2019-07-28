
function populate_commit_info(commit_sha1) {

	$.getJSON('/api/commit-info', {"sha1": commit_sha1}, function (data) {

		var github_link = link("View on GitHub", "https://github.com/pytorch/pytorch/commits/" + commit_sha1);
		var counts_obj = data["payload"]["counts"];

		var items = [
			["Commit ancestry:", github_link],
			["Failed build count:", counts_obj["failed_build_count"]],
			["Matched build count:", counts_obj["matched_build_count"]],
			["Ad hoc reported breakage count:", counts_obj["code_breakage_count"]],
			["Flaky build count:", counts_obj["flaky_build_count"]],
			["Timeout count:", counts_obj["timeout_count"]],
			["Known code breakages count:", counts_obj["known_broken_count"]],

		];

		var stats_table = render_table_vertical_headers(items);

		var analysis_text;

		if (counts_obj["failed_build_count"] == 0) {
			analysis_text = render_tag("span", "No CircleCI builds failed.", {"style": "color: green;"});

		} else if (counts_obj["failed_build_count"] == counts_obj["flaky_build_count"]) {
			analysis_text = render_tag("span", "All of the CircleCI build failures were due to intermittent causes. Consider rerunning them.", {"style": "color: green;"});

		} else if (counts_obj["failed_build_count"] == counts_obj["matched_build_count"]) {
			analysis_text = "All of the CircleCI build failures matched with predefined patterns.";
		} else {
			analysis_text = "Some of the build failure causes weren't determined. Please investigate below.";
		}

		var analysis_summary_items = [
			render_tag("h3", "Analysis"),
			render_tag("p", analysis_text),
		];

		var info_box_items = [
			stats_table,
			analysis_summary_items.join(""),
		];

		var info_box_content = info_box_items.join("");

	        $("#commit-info-box").html(info_box_content);
	});
}


function gen_builds_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title: "Line", field: "match.line_number", width: 100,
				formatter: function(cell, formatterParams, onRendered) {
					return gen_line_number_cell_with_count(cell, cell.getRow().getData()["match"]["line_count"]);
				},
			},
			{title: "Job", width: 300, field: "build.job_name"},
			{title: "Step", width: 250, field: "match.build_step"},
			{title: "Build", field: "build.build_id", formatter: "link",
				formatterParams: {urlPrefix: "/build-details.html?build_id="},
				width: 75,
			},
			{title: "Match (" + render_tag("span", "click to show log context", {"style": "color: #0d0;"}) + ")",
				field: "match.line_text",
				sorter: "string",
				widthGrow: 8,
				formatter: function(cell, formatterParams, onRendered) {
					var row_data = cell.getRow().getData();

					var start_idx = row_data["match"]["span_start"];
					var end_idx = row_data["match"]["span_end"];
							
					return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
				},
				cellClick: function(e, cell) {
					var row_data = cell.getRow().getData();
					var build_id = row_data["build"]["build_id"];
					get_log_text(row_data["match"]["match_id"], STANDARD_LOG_CONTEXT_LINECOUNT);
				},
			},
			{title: "Broken?", field: "breakage", width: 90,
				formatter: function(cell, formatterParams, onRendered) {
					var breakage_obj = cell.getValue();
					if (breakage_obj != null) {
						var img_name = breakage_obj["is_broken"] ? "broken-lightbulb.svg" : "bandaid.svg";
						return "<img src='/images/" + img_name + "' class='brokenness-icon'/>";
					} else {
						return "";
					}
				},
			},
			{title: "Pattern", field: "match.pattern_id", formatter: "link",
				formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="},
				width: 75,
			},
		],
		ajaxURL: data_url,
		ajaxResponse: function(url, params, response) {
			return response.payload;
		},
	});
}


function gen_unmatched_build_list(api_endpoint, div_id) {


	$.getJSON(api_endpoint, function (data) {

		if (data.length == 0) {
			return;
		}

		$("#" + div_id + "-parent").show();


		var table = new Tabulator("#" + div_id, {
			height:"200px",
			layout:"fitColumns",
			placeholder:"No Data Set",
			columns:[
				{title:"Build number", field:"build", formatter: "link", width: 75, formatterParams: {urlPrefix: "/build-details.html?build_id="}},
				{title:"Step", field:"step_name", width: 200},
				{title:"Job", field:"job_name", width: 200},
				{title:"Time", field:"queued_at", width: 150,
					formatter: function(cell, formatterParams, onRendered) {
						var val = cell.getValue();
						return moment(val).fromNow();
					},
				},
				{title:"Branch", field:"branch", width: 150},
				{title:"Broken?", field:"is_broken", formatter:"tickCross", sorter:"boolean",
					formatterParams: {
						allowEmpty: true,
						tickElement:"<img src='/images/broken-lightbulb.svg' class='brokenness-icon'/>",
						crossElement: "<img src='/images/bandaid.svg' class='brokenness-icon'/>",
					},
					width: 90,
				},
			],
			data: data,
		});
	});
}


function rescan_commit(button) {

	var commit_sha1 = get_scrubbed_sha1();
	$(button).prop("disabled", true);
	$("#scan-throbber").show();

        $.post({
		url: "/api/rescan-commit",
		data: {"sha1": commit_sha1},
		success: function( data ) {

			$(button).prop("disabled", false);
			$("#scan-throbber").hide();

			if (data.success) {
				location.reload();
			} else {
				alert("Error: " + data.error.message);
			}
		},
		error: function( data ) {
			$("#scan-throbber").hide();
		},
        });
}


// XXX hack around HUD URL-generation logic which appends "/console"
function get_scrubbed_sha1() {

	var urlParams = new URLSearchParams(window.location.search);
	var commit_sha1 = urlParams.get('sha1');
	var found_slash_index = commit_sha1.indexOf("/");
	if (found_slash_index >= 0) {
		commit_sha1 = commit_sha1.substring(0, found_slash_index);
	}

	return commit_sha1;
}


function main() {

	var commit_sha1 = get_scrubbed_sha1();

	populate_commit_info(commit_sha1);
	gen_builds_table("builds-table", "/api/commit-builds?sha1=" + commit_sha1);

	gen_unmatched_build_list("/api/unmatched-builds-for-commit?sha1=" + commit_sha1, "container-unattributed-failures");
	gen_unmatched_build_list("/api/idiopathic-failed-builds-for-commit?sha1=" + commit_sha1, "container-idiopathic-failures");
	gen_unmatched_build_list("/api/timed-out-builds-for-commit?sha1=" + commit_sha1, "container-timeout-failures");
}
