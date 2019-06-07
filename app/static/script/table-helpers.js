function gen_error_cell_html(cell) {

	var row_data = cell.getRow().getData();

	var start_idx = row_data["span_start"];
	var end_idx = row_data["span_end"];
	return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
}


function get_log_text(build_id, context_linecount) {
	$.getJSON('/api/view-log-context', {"build_id": build_id, "context_linecount": context_linecount}, function (data) {
		console.log(data);

		if (data.success) {

			var table_items = [];
			for (var tuple of data.payload.log_lines) {

				var zero_based_line_number = tuple[0];
				var line_text = tuple[1];

				var one_based_line_number = zero_based_line_number + 1;

				if (zero_based_line_number == data.payload.match_info.line_number) {
					line_text = render_highlighted_line_text(line_text, data.payload.match_info.span_start, data.payload.match_info.span_end);
				}

				var row = [render_tag("div", one_based_line_number, {"class": "line-number"}), render_tag("code", line_text)]
				table_items.push(row);
			}

			$("#myDialog").html( render_table(table_items, {"class": "code-lines"}) );

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
	var cell_html = "<span style='font-family: monospace;'>" + render_highlighted_line_text(line_text, start_idx, end_idx) + "</span>";
	return cell_html;
}


function render_highlighted_line_text(line_text, start_idx, end_idx) {
	return line_text.substring(0, start_idx) + "<span style='background-color: pink;'>" + line_text.substring(start_idx, end_idx) + "</span>" + line_text.substring(end_idx);
}
