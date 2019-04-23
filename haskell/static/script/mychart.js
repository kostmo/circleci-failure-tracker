function main() {

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
