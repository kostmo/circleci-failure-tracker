
function populate_commit_info(commit_sha1) {

	$.getJSON('/api/commit-info', {"sha1": commit_sha1}, function (data) {

		var html = "<dl>"
		html += "<dt>Commit acestry</dt><dd><a href='https://github.com/pytorch/pytorch/commits/" + commit_sha1 + "'>View on GitHub</a></dd>";
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
		{title: "Log", field: "build.build_id", formatter: "link", formatterParams: {urlPrefix: "https://circleci.com/gh/pytorch/pytorch/"}},
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
	    ],
            ajaxURL: data_url,
	    ajaxResponse:function(url, params, response) {
		return response.payload;
	    },
	});
}


function main() {

	var urlParams = new URLSearchParams(window.location.search);
	var commit_sha1 = urlParams.get('sha1');

	populate_commit_info(commit_sha1);
	gen_builds_table("builds-table", "/api/commit-builds?sha1=" + commit_sha1);
}
