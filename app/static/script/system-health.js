

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
			title: {
			    text: 'Line count'
			},
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
			    text: '# of builds'
			}
		    },
		    legend: {
			enabled: false
		    },
        credits: {
            enabled: false
        },
		    series: [{
			name: 'Build count',
			data: mydata,
		    }]
		});

    });
}


function main() {

	gen_log_size_histogram();


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



}

