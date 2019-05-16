function gen_error_cell_html(cell) {

	var line_text = cell.getValue();
	var row_data = cell.getRow().getData();

	var cell_html = "<span style='font-family: monospace;'>" + line_text.substring(0, row_data["span_start"]) +  "<span style='background-color: pink;'>" + line_text.substring(row_data["span_start"], row_data["span_end"]) + "</span>" + line_text.substring(row_data["span_end"]) + "</span>";

	return cell_html;
}
