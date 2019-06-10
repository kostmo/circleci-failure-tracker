


function setup_autocomplete() {

    $( "#branch-name-input" ).autocomplete({
      source: function( request, response ) {
        $.ajax( {
          url: "/api/branch-suggest",
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
	requery_patterns();
      }
    } );
}



function requery_patterns() {

	var branch_name = $( "#branch-name-input" ).val();

	var branches_list = document.getElementById('branch-filter-checkbox').checked ? [branch_name] : [];

	gen_patterns_table(null, branches_list);
}


function main() {

	$("#branch-filter-checkbox").change(function() {
		requery_patterns();
	});

	setup_autocomplete();

	requery_patterns();


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
	subtitle: {
	    text: 'on presumed-stable branches'
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
}
