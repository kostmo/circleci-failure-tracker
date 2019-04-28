
function gen_patterns_table(pattern_id) {

        var ajax_url_query_string = pattern_id == null ? "s" : "?pattern_id=" + pattern_id

        var height = pattern_id == null ? "400px" : null;

	var table = new Tabulator("#patterns-table", {
	    height: height,
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Tags", field:"tags", sorter:"string"},
		{title:"Pattern", field:"pattern", sorter:"string", widthGrow: 3},
		{title:"Description", field:"description", sorter:"string", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id=", urlField: "id"}, widthGrow: 2},
		{title:"Frequency", field:"frequency", sorter:"number", align:"center", width: 75},
		{title:"Last Occurrence", field:"last", sorter:"datetime", align:"center"},
		{title:"Regex", field:"is_regex", align:"center", formatter:"tickCross", sorter:"boolean", formatterParams: {crossElement: false}, width: 75},
	    ],
            ajaxURL: "api/pattern" + ajax_url_query_string,
	});
}


function gen_error_cell_html(cell) {

    var line_text = cell.getValue();
    var row_data = cell.getRow().getData();

    var cell_html = line_text.substring(0, row_data["span_start"]) +  "<span style='background-color: pink;'>" + line_text.substring(row_data["span_start"], row_data["span_end"]) + "</span>" + line_text.substring(row_data["span_end"]);

    return cell_html;
}

function pattern_details_page() {

	var urlParams = new URLSearchParams(window.location.search);
	var pattern_id = urlParams.get('pattern_id');


        gen_patterns_table(pattern_id);


	var table = new Tabulator("#pattern-matches-table", {
	    height:"300px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build number", field:"build_number", formatter: "link", width: 75, formatterParams: {urlPrefix: "https://circleci.com/gh/pytorch/pytorch/"}},
		{title:"Build step", field:"build_step", sorter:"string", widthGrow: 2},
		{title:"Line number", field:"line_number", sorter:"number", width: 75},
		{title:"Line text", field:"line_text", sorter:"string", widthGrow: 8, formatter:function(cell, formatterParams, onRendered) {
			return gen_error_cell_html(cell);
		  },
			cellClick: function(e, cell){
			    $("#error-display").html(gen_error_cell_html(cell));
		    },
	        },
	    ],
            ajaxURL: "/api/pattern-matches?pattern_id=" + pattern_id,
	});
}


function main() {

   gen_patterns_table(null);

   $.getJSON('api/job', function (data) {

      Highcharts.chart('container-job-failures', {

        chart: {
            type: 'bar'
        },
        title: {
            text: 'Build failure counts by job name'
        },
        xAxis: {
            categories: ['Job'],
            title: {
                text: null
            }
        },
        yAxis: {
            min: 0,
            title: {
                text: 'Failure count',
                align: 'high'
            },
            labels: {
                overflow: 'justify'
            }
        },
        plotOptions: {
            bar: {
                dataLabels: {
                    enabled: true
                }
            }
        },
        legend: {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'top',
            x: -40,
            y: 80,
            floating: true,
            borderWidth: 1,
            backgroundColor: ((Highcharts.theme && Highcharts.theme.legendBackgroundColor) || '#FFFFFF'),
            shadow: true
        },
        credits: {
            enabled: false
        },
        series: data.rows,
      });

   });


   $.getJSON('api/step', function (data) {


      Highcharts.chart('container-step-failures', {
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: null,
            plotShadow: false,
            type: 'pie'
        },
        title: {
            text: 'Failures by step name'
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
            data: data.rows,
         }]
      });

   });


   $.getJSON('api/failed-commits-by-day', function (data) {

        var rows = [];

        $.each(data.rows, function( index, value ) {

                rows.push([Date.parse(value[0]), value[1]]);
        });

      Highcharts.chart('container-failed-commits-by-day', {

           chart: {
                type: 'spline'
            },
            title: {
                text: 'Failed commits by day'
            },
            xAxis: {
                type: 'datetime',
                dateTimeLabelFormats: { // don't display the dummy year
                    month: '%e. %b',
                    year: '%b'
                },
                title: {
                    text: 'Date'
                }
            },
            yAxis: {
                title: {
                    text: 'Failure count'
                },
                min: 0
            },
            tooltip: {
                headerFormat: '<b>{series.name}</b><br>',
                pointFormat: '{point.x:%e. %b}: {point.y:.2f} m'
            },

            plotOptions: {
                spline: {
                    marker: {
                        enabled: true
                    }
                }
            },

        credits: {
            enabled: false
        },
        series: [{
            name: "Broken commits",
            data: rows,
            }],
      });
   });
}
