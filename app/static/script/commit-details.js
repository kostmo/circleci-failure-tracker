
function populate_commit_info(commit_sha1) {

	$.getJSON('/api/commit-info', {"sha1": commit_sha1}, function (data) {

		var github_link = "<a href='https://github.com/pytorch/pytorch/commits/" + commit_sha1 + "'>View on GitHub</a>"
		var html = "<dl>"
		html += "<dt>Commit ancestry</dt><dd>" + github_link + "</dd>";
		html += "<dt>Broken build count:</dt><dd>" + data["payload"]["failed_build_count"] + "</dd>";

	        $("#commit-info-box").html(html);

		populate_breakage_form("submission_button_placeholder", commit_sha1, data);
	});
}


function gen_builds_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
	    height:"300px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title: "Line", field: "match.line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return (cell.getValue() + 1) + " / " + cell.getRow().getData()["match"]["line_count"];
		  }},
		{title: "Job", width: 300, field: "build.job_name"},
		{title: "Step", width: 250, field: "match.build_step"},
		{title: "Build", field: "build.build_id", formatter: "link", formatterParams: {urlPrefix: "/build-details.html?build_id="}, width: 75},
		{title: "Line text", field: "match.line_text", sorter: "string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
			var row_data = cell.getRow().getData();

			var start_idx = row_data["match"]["span_start"];
			var end_idx = row_data["match"]["span_end"];
					
			return gen_error_cell_html_parameterized(cell, start_idx, end_idx);

		  },
			cellClick: function(e, cell){
			    $("#error-display").html(gen_error_cell_html(cell));
		    },
	        },
		{title: "Pattern", field: "match.pattern_id", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="}, width: 75},
	    ],
            ajaxURL: data_url,
	    ajaxResponse:function(url, params, response) {
		return response.payload;
	    },
	});
}


function gen_unmatched_build_list(api_endpoint, div_id) {

	var table = new Tabulator("#" + div_id, {
	    height:"200px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build number", field:"build", formatter: "link", width: 75, formatterParams: {urlPrefix: "/build-details.html?build_id="}},
		{title:"Step", field:"step_name", width: 200},
		{title:"Job", field:"job_name", width: 200},
		{title:"Time", field:"queued_at", width: 150},
		{title:"Branch", field:"branch", width: 150},
	    ],
            ajaxURL: api_endpoint,
	});
}


function main() {

	var urlParams = new URLSearchParams(window.location.search);

	// XXX hack around HUD URL-generation logic which appends "/console"
	var commit_sha1 = urlParams.get('sha1');
	var found_slash_index = commit_sha1.indexOf("/");
	if (found_slash_index >= 0) {
		commit_sha1 = commit_sha1.substring(0, found_slash_index);
		console.log("Fixed up sha1 parameter!");
	}

	populate_commit_info(commit_sha1);
	gen_builds_table("builds-table", "/api/commit-builds?sha1=" + commit_sha1);

	gen_unmatched_build_list("/api/unmatched-builds-for-commit?sha1=" + commit_sha1, "container-unattributed-failures");
	gen_unmatched_build_list("/api/idiopathic-failed-builds-for-commit?sha1=" + commit_sha1, "container-idiopathic-failures");
}
