
function display_stats() {

	$.getJSON('/api/summary', function (data) {
		$("#unattributed-builds-count-container").html("Count: <b>" + data["unattributed_failed_builds"] + "</b>");
		$("#idiopathic-builds-count-container").html("Count: <b>" + data["idiopathic_build_failures"] + "</b>");
	});
}

function gen_build_list(api_endpoint, div_id) {

	var table = new Tabulator("#" + div_id, {
	    height:"200px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build number", field:"build_number", formatter: "link", width: 75, formatterParams: {urlPrefix: "/build-details.html?build_id="}},
		{title:"Branch", field:"branch", formatter: "string"},
	    ],
            ajaxURL: api_endpoint,
	});
}



function main() {

	gen_build_list("/api/idiopathic-failed-builds", "container-idiopathic-failures");
	gen_build_list("/api/unmatched-builds", "container-unattributed-failures");


	display_stats();

}
