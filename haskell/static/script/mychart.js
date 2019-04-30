
function gen_patterns_table(pattern_id) {

        var ajax_url_query_string = pattern_id == null ? "s" : "?pattern_id=" + pattern_id

        var height = pattern_id == null ? "400px" : null;

	var table = new Tabulator("#patterns-table", {
	    height: height,
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Tags", field:"tags", sorter:"string"},
		{title:"Regex", field:"is_regex", align:"center", formatter:"tickCross", sorter:"boolean", formatterParams: {crossElement: false}, width: 75},
		{title:"Pattern", field:"pattern", sorter:"string", widthGrow: 3, formatter: function(cell, formatterParams, onRendered) {
			return "<code>" + cell.getValue() + "</code>";
		  },
                },
		{title:"Description", field:"description", sorter:"string", formatter: "link", formatterParams: {urlPrefix: "/pattern-details.html?pattern_id=", urlField: "id"}, widthGrow: 2},
		{title:"Frequency", field:"frequency", sorter:"number", align:"center", width: 75},
		{title:"Last Occurrence", field:"last", sorter:"datetime", align:"center"},
	    ],
            ajaxURL: "/api/pattern" + ajax_url_query_string,
	});
}


function gen_error_cell_html(cell) {

    var line_text = cell.getValue();
    var row_data = cell.getRow().getData();

    var cell_html = line_text.substring(0, row_data["span_start"]) +  "<span style='background-color: pink;'>" + line_text.substring(row_data["span_start"], row_data["span_end"]) + "</span>" + line_text.substring(row_data["span_end"]);

    return cell_html;
}

function gen_build_list(api_endpoint, div_id) {

	var table = new Tabulator("#" + div_id, {
	    height:"200px",
	    layout:"fitColumns",
	    placeholder:"No Data Set",
	    columns:[
		{title:"Build number", field:"build_number", formatter: "link", width: 75, formatterParams: {urlPrefix: "https://circleci.com/gh/pytorch/pytorch/"}},
	    ],
            ajaxURL: api_endpoint,
	});
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
		{title:"Line number", field:"line_number", width: 100, formatter: function(cell, formatterParams, onRendered) {
			return cell.getValue() + " / " + cell.getRow().getData()["line_count"];
		  }},
		{title:"Line text", field:"line_text", sorter:"string", widthGrow: 8, formatter: function(cell, formatterParams, onRendered) {
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


function gen_log_size_histogram() {


   $.getJSON('/api/log-size-histogram', function (mydata) {

		Highcharts.chart('line-histogram-container', {
		    chart: {
			type: 'column'
		    },
		    title: {
			text: 'Frequency of log line counts'
		    },
		    xAxis: {
			type: 'category',
			labels: {
			    rotation: -45,
			    style: {
				fontSize: '13px',
				fontFamily: 'Verdana, sans-serif'
			    }
			}
		    },
		    yAxis: {
			min: 0,
			title: {
			    text: 'Line count'
			}
		    },
		    legend: {
			enabled: false
		    },
		    series: [{
			name: 'Line counts',
			data: mydata,
			dataLabels: {
			    enabled: true,
			    rotation: -90,
			    color: '#FFFFFF',
			    align: 'right',
			    format: '{point.y:.1f}', // one decimal
			    y: 10, // 10 pixels down from the top
			    style: {
				fontSize: '13px',
				fontFamily: 'Verdana, sans-serif'
			    }
			}
		    }]
		});

    });
}


function gen_summary_nested_pie() {


   $.getJSON('/api/summary', function (mydata) {

		// Splice in transparent for the center circle
		Highcharts.getOptions().colors.splice(0, 0, 'transparent');

		Highcharts.chart('container-visited-fraction', {

		    chart: {
			height: '100%'
		    },
		    title: {
			text: 'Failure causes'
		    },
		credits: {
		    enabled: false
		},
		    series: gen_sunburst_data_series(mydata),
		    tooltip: {
			headerFormat: "",
			pointFormat: '<b>{point.value}</b> in <b>{point.name}</b>'
		    }
		});
    });
}

function gen_sunburst_data_series(data) {

		var chartdata = [{
		    id: '0.0',
		    parent: '',
		    name: 'Failures'

		}, {
		    id: '1.1',
		    parent: '0.0',
		    name: 'Unvisited',
		    value: data["failed_builds"] - data["visited_builds"],
		}, {
		    id: '1.2',
		    parent: '0.0',
		    name: 'Visited',
		    value: data["visited_builds"],
		}, {
		    id: '2.1',
		    parent: '1.2',
		    name: 'Cause available',
		    value: data["explained_failures"],
		}, {
		    id: '2.2',
		    parent: '1.2',
		    name: 'Cause unvailable',
		    value: data["visited_builds"] - data["explained_failures"],
		}, {
		    id: '3.1',
		    parent: '2.1',
		    name: 'Timeouts',
		    value: data["timed_out_steps"],
		}, {
		    id: '3.2',
		    parent: '2.1',
		    name: 'Logs available',
		    value: data["explained_failures"] - data["timed_out_steps"],
		}, {
		    id: '4.1',
		    parent: '3.2',
		    name: 'Match found',
		    value: data["steps_with_a_match"],
		},


		];


	return [{
			type: "sunburst",
			data: chartdata,
			allowDrillToNode: true,
			cursor: 'pointer',
			dataLabels: {
			    format: '{point.name}',
			    filter: {
				property: 'innerArcLength',
				operator: '>',
				value: 16
			    }
			},
			levels: [{
			    level: 1,
			    levelIsConstant: false,
			}, {
			    level: 2,
			    colorByPoint: true,
			},
			{
			    level: 3,
			    colorByPoint: true,
			}, {
			    level: 4,
			    colorByPoint: true,
			}]

		    }];
}


function setup_autocomplete() {


    $( "#build-step-applicability-input" ).autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "/api/steps",
          dataType: "json",
          data: {
            term: request.term
          },
          success: function( data ) {
            response( data );
          }
        } );
      },
      minLength: 1,
      select: function( event, ui ) {
        console.log( "Selected: " + ui.item.value + " aka " + ui.item.id );
      }
    } );




    $( "#pattern-tag-input" ).autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "/api/tags",
          dataType: "json",
          data: {
            term: request.term
          },
          success: function( data ) {
            response( data );
          }
        } );
      },
      minLength: 1,
      select: function( event, ui ) {
        console.log( "Selected: " + ui.item.value + " aka " + ui.item.id );
      }
    } );
}



function submit_pattern() {

        $.get( {
          url: "/api/new-pattern-test",
          data: {
            is_regex: $('#is-regex-checkbox').is(":checked"),
            is_nondeterministic: $('#is-nondeterministic-checkbox').is(":checked"),
            build_num: $('#test-build-id').val(),
            pattern: $('#input-pattern-text').val(),
            description: $('#input-pattern-description').val(),
            tags: $('#pattern-tag-input').val(),
            applicable_steps: $('#build-step-applicability-input').val(),
          },
          success: function( data ) {
		console.log("Results: " + JSON.stringify(data));

		$("#test-match-results-container").html( JSON.stringify(data) );

          }
        } );
}


function get_random_build(destination_field_id) {

        $.get( {
          url: "/api/random-scannable-build",
          success: function( data ) {
            $("#" + destination_field_id).val(data["build_number"]);

          }
        } );
}


function main() {


	setup_autocomplete();

	gen_log_size_histogram();
	gen_patterns_table(null);



	gen_build_list("/api/idiopathic-failed-builds", "container-idiopathic-failures");
	gen_build_list("/api/unmatched-builds", "container-unattributed-failures");


	gen_summary_nested_pie();

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


   $.getJSON('/api/step', function (data) {

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







   $.getJSON('/api/disk', function (mydata) {

      Highcharts.chart('container-disk-space', {
        chart: {
            plotBackgroundColor: null,
            plotBorderWidth: null,
            plotShadow: false,
            type: 'pie'
        },
        title: {
            text: 'Disk consumption for logs'
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
            name: 'Disk',
            colorByPoint: true,
            data: mydata,
         }]
      });

   });





   $.getJSON('/api/failed-commits-by-day', function (data) {

        var rows = [];

        $.each(data.rows, function( index, value ) {

                rows.push([Date.parse(value[0]), value[1]]);
        });

      Highcharts.chart('container-failed-commits-by-day', {

           chart: {
                type: 'line'
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
                line: {
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
