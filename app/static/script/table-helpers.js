var STANDARD_LOG_CONTEXT_LINECOUNT = 25;


function gen_error_cell_html(cell) {

	var row_data = cell.getRow().getData();

	var start_idx = row_data["span_start"];
	var end_idx = row_data["span_end"];
	return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
}


function get_log_text(match_id, context_linecount) {
	$.getJSON('/api/view-log-context', {"match_id": match_id, "context_linecount": context_linecount}, function (data) {

		if (data.success) {

			var table_items = [];
			for (var tuple of data.payload.log_lines) {

				var zero_based_line_number = tuple[0];
				var line_text = tuple[1];

				var one_based_line_number = zero_based_line_number + 1;

				var formatted_line_text;
				if (zero_based_line_number == data.payload.match_info.line_number) {
					formatted_line_text = render_highlighted_line_text(line_text, data.payload.match_info.span.start, data.payload.match_info.span.end);
				} else {
					formatted_line_text = line_text;
				}

				var row = [render_tag("div", one_based_line_number, {"class": "line-number"}), render_tag("code", formatted_line_text)]
				table_items.push(row);
			}

			var dialog_content = [
				render_tag("div", "<a href='javascript: document.getElementById(\"myDialog\").close();'>Press <i>Esc</i> to close</a>", {"style": "color: gray; text-align: right; float: right;"}),
				render_tag("div", link("Full plaintext log", "/api/view-log-full?build_id=" + data.payload.universal_build_id) + " | " + link("on CircleCI", "https://circleci.com/gh/pytorch/pytorch/" + data.payload.build_number, true)),
				render_table(table_items, {"class": "code-lines"}, null, false),
			];

			$("#myDialog").html(dialog_content.join(""));

			var dialog = document.getElementById("myDialog");
			dialog.addEventListener('click', function (event) {
					var rect = dialog.getBoundingClientRect();
					var isInDialog=(rect.top <= event.clientY && event.clientY <= rect.top + rect.height
					&& rect.left <= event.clientX && event.clientX <= rect.left + rect.width);
					if (!isInDialog) {
					dialog.close();
				}
			});


			document.getElementById("myDialog").showModal(); 

		} else {
			var proceed = confirm("Need to login first...");
			if (proceed) {
				window.location.href = data.error.details.login_url;
			}
		}
	});
}


function gen_line_number_cell_with_count(cell, line_count) {

	var distance_from_start = cell.getValue() + 1;
	var distance_from_end = line_count - distance_from_start;
	if (distance_from_end == 0) {
		return "last";
	} else {
		return distance_from_start + " / " + line_count + " (-" + distance_from_end + ")";
	}
}


function gen_line_number_cell(cell) {
	var line_count = cell.getRow().getData()["line_count"];
	return gen_line_number_cell_with_count(cell, line_count);
}


function gen_error_cell_html_parameterized(cell, start_idx, end_idx) {
	var line_text = cell.getValue();
	var cell_html = render_tag("span", render_highlighted_line_text(line_text, start_idx, end_idx), {"style": "font-family: monospace;"});
	return cell_html;
}


function render_highlighted_line_text(line_text, start_idx, end_idx) {
	return line_text.substring(0, start_idx) + render_tag("span", line_text.substring(start_idx, end_idx), {"class": "pattern-match-highlight"}) + line_text.substring(end_idx);
}
