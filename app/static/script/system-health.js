
function gen_log_size_histogram() {

	$.getJSON('/api/log-storage-stats', function (data) {
		var html = "<dl>";
		$.each(data, function( key, value ) {

			var value_content = key == "total_bytes" ? Humanize.fileSize(value, 1) + " (uncompressed)" : Humanize.compactInteger(value, 1)
			html += render_pair(key, value_content);
		});
		html += "</dl>";

		$("#log-size-stats-container").html(html)
	});


	$.getJSON('/api/log-lines-histogram', function (mydata) {

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



	$.getJSON('/api/log-size-histogram', function (mydata) {

		Highcharts.chart('byte-histogram-container', {
		    chart: {
			type: 'column'
		    },
		    title: {
			text: 'Frequency of log byte counts'
		    },
		    xAxis: {
			type: 'category',
			title: {
			    text: 'Byte count'
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

}

