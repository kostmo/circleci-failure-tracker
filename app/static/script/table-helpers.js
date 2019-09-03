const STANDARD_LOG_CONTEXT_LINECOUNT = 25;


function gen_error_cell_html(cell) {

	const row_data = cell.getRow().getData();

	const start_idx = row_data["span_start"];
	const end_idx = row_data["span_end"];
	return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
}


function revealHiddenLogContext(link_container_id) {

	$(".hidden-preloaded-log-line").show();
	$("#" + link_container_id).html(render_tag("i", "extra context shown"));
}


function renderLogLineTableWithOffset(table_items, matched_line_number, context_linecount) {

	var content = "";
	for (var row of table_items) {

		const zero_based_line_number = row[0];

		const one_based_line_number = zero_based_line_number + 1;

		const repackaged_row = [
			render_tag("div", one_based_line_number, {"class": "line-number"}),
			row[1],
		];
		
		const row_content = render_table_row(repackaged_row, "td");

		const is_hidden_context = matched_line_number - zero_based_line_number > context_linecount;

		const attrs = {};
		if (is_hidden_context) {
			attrs["class"] = "hidden-preloaded-log-line";
		}

		content += render_tag("tr", row_content, attrs);
	}

	return render_tag("table", render_tag("tbody", content));
}


function get_log_text(match_id, context_linecount) {
	$.getJSON('/api/view-log-context', {"match_id": match_id, "context_linecount": context_linecount}, function (data) {

		if (data.success) {

			const table_items = [];
			for (var tuple of data.payload.log_lines) {

				const zero_based_line_number = tuple[0];
				const line_text = tuple[1];


				var formatted_line_text;
				if (zero_based_line_number == data.payload.match_info.line_number) {
					formatted_line_text = render_highlighted_line_text(line_text, data.payload.match_info.span.start, data.payload.match_info.span.end);
				} else {
					formatted_line_text = line_text;
				}


				const row = [zero_based_line_number, render_tag("code", formatted_line_text)]
				table_items.push(row);
			}


			const left_corner_links = [
				render_tag("span", link("+1000 lines", "javascript: revealHiddenLogContext('context-reveal-link-container');"), {"id": "context-reveal-link-container"}),
				link("Full plaintext log", "/api/view-log-full?build_id=" + data.payload.universal_build_id),
				link("on CircleCI", "https://circleci.com/gh/pytorch/pytorch/" + data.payload.build_number, true)
			]


			const dialog_content = [
				render_tag("div", link(
					"Press " + render_tag("i", "Esc") + " to close",
					"javascript: document.getElementById('myDialog').close();"
				), {"style": "color: gray; text-align: right; float: right;"}),
				render_tag("div", left_corner_links.join(" | ")),
				render_tag("div", renderLogLineTableWithOffset(table_items, data.payload.match_info.line_number, context_linecount)),
			];

			$("#myDialog").html(dialog_content.join(""));

			const dialog = document.getElementById("myDialog");
			dialog.addEventListener('click',
				function (event) {
					const rect = dialog.getBoundingClientRect();
					const isInDialog = (rect.top <= event.clientY && event.clientY <= rect.top + rect.height
						&& rect.left <= event.clientX && event.clientX <= rect.left + rect.width);
					if (!isInDialog) {
					dialog.close();
				}
			});


			document.getElementById("myDialog").showModal(); 

		} else {
			const proceed = confirm("Need to login first...");
			if (proceed) {
				window.location.href = data.error.details.login_url;
			}
		}
	});
}


function gen_line_number_cell_with_count(cell, line_count) {

	const distance_from_start = cell.getValue() + 1;
	const distance_from_end = line_count - distance_from_start;
	if (distance_from_end == 0) {
		return "last";
	} else {
		return distance_from_start + " / " + line_count + " (-" + distance_from_end + ")";
	}
}


function gen_line_number_cell(cell) {
	const line_count = cell.getRow().getData()["line_count"];
	return gen_line_number_cell_with_count(cell, line_count);
}


function gen_error_cell_html_parameterized(cell, start_idx, end_idx) {
	const line_text = cell.getValue();
	const cell_html = render_tag("span", render_highlighted_line_text(line_text, start_idx, end_idx), {"style": "font-family: monospace;"});
	return cell_html;
}


function render_highlighted_line_text(line_text, start_idx, end_idx) {
	return line_text.substring(0, start_idx) + render_tag("span", line_text.substring(start_idx, end_idx), {"class": "pattern-match-highlight"}) + line_text.substring(end_idx);
}
