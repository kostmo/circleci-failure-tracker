function gen_step_failures_chart(container_id, pattern_id) {

   $.getJSON('/api/pattern-step-occurrences', {"pattern_id": pattern_id}, function (data) {

      Highcharts.chart(container_id, {
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: null,
            plotShadow: false,
            type: 'pie'
        },
        title: {
            text: 'Failures by step name'
        },
	subtitle: {
	    text: 'for pattern ' + pattern_id
	},
        tooltip: {
            pointFormat: '{series.name}: <b>{point.percentage:.1f}%</b>'
        },
        plotOptions: {
            pie: {
                allowPointSelect: true,
                cursor: 'pointer',
                dataLabels: {
                    enabled: true,
                    format: '<b>{point.name}</b>: {point.percentage:.1f} %',
                    style: {
                        color: (Highcharts.theme && Highcharts.theme.contrastTextColor) || 'black'
                    }
                }
            }
        },
        credits: {
            enabled: false
        },
        series: [{
            name: 'Steps',
            colorByPoint: true,
            data: data,
         }]
      });
   });
}


function gen_matches_table(element_id, data_url) {

	var table = new Tabulator("#" + element_id, {
		height:"300px",
		layout:"fitColumns",
		placeholder:"No Data Set",
		columns:[
			{title:"Build number", field:"build_number", formatter: "link", width: 75,
				formatterParams: {urlPrefix: "/build-details.html?build_id="},
			},
			{title:"Revision", field:"vcs_revision",
				formatter: function(cell, formatterParams, onRendered) {
					return render_tag("code", link(cell.getValue().substring(0, 7), "/commit-details.html?sha1=" + cell.getValue()));
				},
				width: 75,
			},
			{title:"Branch", field:"branch", sorter:"string", widthGrow: 1},
			{title:"Job", field:"job_name", sorter:"string", widthGrow: 3},
			{title:"Build step", field:"build_step", sorter:"string", widthGrow: 2},
			{title:"Line", field:"line_number", width: 100,
				formatter: function(cell, formatterParams, onRendered) {
					return gen_line_number_cell(cell);
				},
			},
			{title: "Match (" + render_tag("span", "click to show log context", {"style": "color: #0d0;"}) + ")",
				field:"line_text", sorter:"string", widthGrow: 8,
				formatter: function(cell, formatterParams, onRendered) {
					return gen_error_cell_html(cell);
				},
				cellClick: function(e, cell){
					var row_data = cell.getRow().getData();
					var match_id = row_data["match_id"];
					get_log_text(match_id, STANDARD_LOG_CONTEXT_LINECOUNT);
				},
			},
		],
		ajaxURL: data_url,
	});
}


function gen_all_matches_table(pattern_id) {
	gen_matches_table("all-pattern-matches-table", "/api/pattern-matches?pattern_id=" + pattern_id);
}


function gen_best_matches_table(pattern_id) {
	gen_matches_table("best-pattern-matches-table", "/api/best-pattern-matches?pattern_id=" + pattern_id);
}


function main() {

	var urlParams = new URLSearchParams(window.location.search);
	var pattern_id = urlParams.get('pattern_id');

        gen_patterns_table(pattern_id, false, []);

        gen_best_matches_table(pattern_id);
        gen_all_matches_table(pattern_id);

	gen_step_failures_chart('container-step-failures', pattern_id);
}

