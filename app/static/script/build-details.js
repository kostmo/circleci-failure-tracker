function gen_builds_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
	    height:"300px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build step", field: "build_step", sorter: "string", widthGrow: 2},
		{title:"Line", field: "line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return (cell.getValue() + 1) + " / " + cell.getRow().getData()["line_count"];
		  }},
		{title:"Line text", field: "line_text", sorter: "string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
			return gen_error_cell_html(cell);
		  },
			cellClick: function(e, cell){
			    $("#error-display").html(gen_error_cell_html(cell));
		    },
	        },
	    ],
            ajaxURL: data_url,
	});
}


function populate_build_info(build_id) {

	var html = "View log <a href='https://circleci.com/gh/pytorch/pytorch/" + build_id + "'>on CircleCI</a>";

        $("#build-info-box").html(html);
}


function main() {

	var urlParams = new URLSearchParams(window.location.search);
	var build_id = urlParams.get('build_id');

	populate_build_info(build_id);
	gen_builds_table("all-build-matches-table", "/api/build-pattern-matches?build_id=" + build_id);
}

