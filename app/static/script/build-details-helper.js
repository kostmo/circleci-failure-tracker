
function render_build_link_cell(universal_build_number, icon_url, provider_build_number) {

	const url = "/build-details.html?build_id=" + universal_build_number;
	return link('<img src="' + icon_url + '?s=16" style="vertical-align: middle"/>', "https://circleci.com/gh/pytorch/pytorch/" + provider_build_number) + ' ' + render_tag("span", link(provider_build_number, url), {"style": "vertical-align: middle"})
}


function gen_builds_table_columns(include_timeout_column) {

	const columns = [
		{title: "Line", field: "match.line_number", width: 100,
			formatter: function(cell, formatterParams, onRendered) {

				const line_count = cell.getRow().getData()["match"]["line_count"];
				if (line_count < 0) {

					return "";
				} else {
					return gen_line_number_cell_with_count(cell, cell.getRow().getData()["match"]["line_count"]);
				}
			},
		},
		{title: "Job", width: 300, field: "build.build_record.job_name",
			tooltip: "Click to copy",
			cellClick: function(e, cell) {

				navigator.clipboard.writeText( cell.getValue() )
				  .then(() => {
				    // Success!
				  })
				  .catch(err => {
				    console.log('Something went wrong', err);
				  });
			},
		},
		{title: "Step", width: 250, field: "match.build_step"},
		{title: "Build", field: "build.build_record.build_id",
			formatter: function(cell, formatterParams, onRendered) {

				const row_data = cell.getRow().getData();

				const provider_build_number = row_data["build"]["build_record"]["build_id"];
				const universal_build_number = row_data["build"]["universal_build"]["db_id"];

				return render_build_link_cell(universal_build_number, row_data["provider"]["record"]["icon_url"], provider_build_number);
			},
			tooltip: function(cell) {
				const row_data = cell.getRow().getData();
				return row_data["provider"]["record"]["label"];
			},
			width: 90,
		},
		{title: "Match (" + render_tag("span", "click to show log context", {"style": "color: #0d0;"}) + ")",
			field: "match.line_text",
			sorter: "string",
			widthGrow: 8,
			formatter: function(cell, formatterParams, onRendered) {

				const row_data = cell.getRow().getData();

				if (row_data["failure_mode"]["is_timeout"]) {

					return "<span style='font-style: italic; color: #00b;'>&lt;timeout&gt;</span>";
				} else {
					const start_idx = row_data["match"]["span_start"];
					const end_idx = row_data["match"]["span_end"];
							
					return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
				}

			},
			cellClick: function(e, cell) {

				const row_data = cell.getRow().getData();
				if (row_data["match"]["pattern_id"] >= 0) {

					const build_id = row_data["build"]["build_id"];
					get_log_text(row_data["match"]["match_id"], STANDARD_LOG_CONTEXT_LINECOUNT);
				}
			},
		},
		{title: "Pattern", field: "match.pattern_id",
			formatter: function(cell, formatterParams, onRendered) {

				if (cell.getValue() < 0) {

					return "";
				} else {
					return link(cell.getValue(), "/pattern-details.html?pattern_id=" + cell.getValue());
				}
			},
			width: 75,
		},
	];

	if (include_timeout_column) {

		const col_def = {
			title: "Pattern", field: "match.pattern_id", formatter: "link",
			formatterParams: {urlPrefix: "/pattern-details.html?pattern_id="},
			width: 75,
		};
		columns.push(col_def);
	}

	return columns;
}
