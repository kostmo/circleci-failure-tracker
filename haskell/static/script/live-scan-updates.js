
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



