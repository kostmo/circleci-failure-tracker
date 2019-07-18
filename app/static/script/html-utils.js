function post_modification(api_endpoint, data_dict) {

	$.post( {
		url: api_endpoint,
		data: data_dict,
		success: function( data ) {
			if (data.success) {
				console.log("Result: " + data.payload);
				location.reload();
			} else {
				if (data.error.details.authentication_failed) {
					alert("Not logged in: " + data.error.message);
					window.location.href = data.error.details.login_url;
				} else if (data.error.details.database_failed) {
					alert("Database error: " + data.error.message);
				} else {
					alert("Unknown error: " + data.error.message);
				}
			}
		}
	});
}


function get_commit_subject(msg) {
	return msg.split(/\r?\n/)[0];
}


function render_tag(tag, content, attributes) {

	var attributes = attributes || {};
	var kv_pairs = [tag];

	$.each(attributes, function(key, value) {
		kv_pairs.push(key + '="' + value + '"')
	});

	return "<" + kv_pairs.join(" ") + ">" + content + "</" + tag + ">";
}


function sha1_link(full_sha1) {
	return render_tag("code", link(full_sha1.substring(0, 7), "/commit-details.html?sha1=" + full_sha1));
}


function link(text, url, new_window) {

	var prop_dict = {"href": url}
	if (new_window) {
		prop_dict["target"] = "_blank";
	}

	return render_tag("a", text, prop_dict);
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


function render_table_vertical_headers(rows, attrs) {

	var content = "";
	for (var row of rows) {
		var row_content = "";

		for (var i=0; i<row.length; i++) {
			var item = row[i];
			var cell = i ? render_tag("td", item) : render_tag("th", item, {"style": "text-align: right;"});
			row_content += cell;
		}

		content += render_tag("tr", row_content);
	}

	return render_tag("table", render_tag("tbody", content), attrs);
}


function render_table(rows, attrs) {

	var content = "";
	for (var row of rows) {
		var row_content = "";
		for (var item of row) {
			row_content += render_tag("td", item);
		}
		content += render_tag("tr", row_content);
	}

	return render_tag("table", render_tag("tbody", content), attrs);
}
