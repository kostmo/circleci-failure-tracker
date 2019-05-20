function gen_error_cell_html(cell) {

	var row_data = cell.getRow().getData();

	var start_idx = row_data["span_start"];
	var end_idx = row_data["span_end"];
	return gen_error_cell_html_parameterized(cell, start_idx, end_idx);
}


function gen_error_cell_html_parameterized(cell, start_idx, end_idx) {
	var line_text = cell.getValue();

	var cell_html = "<span style='font-family: monospace;'>" + line_text.substring(0, start_idx) + "<span style='background-color: pink;'>" + line_text.substring(start_idx, end_idx) + "</span>" + line_text.substring(end_idx) + "</span>";

	return cell_html;
}
