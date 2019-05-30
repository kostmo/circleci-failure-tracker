

function gen_builds_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title: "Revision", field: "sha1", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return '<code><a href="/commit-details.html?sha1=' + cell.getValue() + '">' + cell.getValue().substring(0, 7) + '</a></code>';
		  }},
		{title: "Message", field: "description", sorter: "string", widthGrow: 2},
		{title: "Time", field: "created_at", width: 100},
	    ],
            ajaxURL: data_url,
	});
}




function main() {

	gen_builds_table("status-postings-table", "/api/posted-statuses");

}

