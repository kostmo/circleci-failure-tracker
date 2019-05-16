

function gen_build_list(api_endpoint, div_id) {

	var table = new Tabulator("#" + div_id, {
	    height:"200px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build number", field:"build_number", formatter: "link", width: 75, formatterParams: {urlPrefix: "https://circleci.com/gh/pytorch/pytorch/"}},
		{title:"Branch", field:"branch", formatter: "string"},
	    ],
            ajaxURL: api_endpoint,
	});
}



function main() {

	gen_build_list("/api/idiopathic-failed-builds", "container-idiopathic-failures");
	gen_build_list("/api/unmatched-builds", "container-unattributed-failures");


}
