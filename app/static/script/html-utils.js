function render_tag(tag, content) {
	return "<" + tag + ">" + content + "</" + tag + ">";
}

function render_pair(term, definition) {
	return render_tag("dt", term) + render_tag("dd", definition);
}




function render_list(items) {

	var content = "";
	for (var item of items) {
		content += render_tag("li", item);
	}

	return render_tag("ul", content);
}


function render_table(rows) {

	var content = "";
	for (var row of rows) {
		var row_content = "";
		for (var item of row) {
			row_content += render_tag("td", item);
		}
		content += render_tag("tr", row_content);
	}

	return render_tag("table", render_tag("tbody", content));
}
