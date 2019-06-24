
function get_timeline_data(offset, count) {

	$.getJSON('/api/master-timeline', {"offset": offset, "count": count}, function (mydata) {
		gen_timeline_table("master-timeline-table", mydata);
	});
}


// global
var build_failures_by_commit;

function gen_timeline_table(element_id, fetched_data) {

	var column_list = [];

	var commit_column_definition = {
		title: "Commit",
		field: "commit",
		headerVertical: false,
		formatter: function(cell, formatterParams, onRendered) {
			return render_tag("code", link(cell.getValue().substring(0, 7), "/commit-details.html?sha1=" + cell.getValue()));
		},
		minWidth: 90,
		width: 90,
		resizable: true,
		headerSort: false,
		frozen: true,
	};

	column_list.push(commit_column_definition);

	var filtered_column_names = fetched_data.columns.filter(x => !x.startsWith("binary"));
	filtered_column_names.sort();

	for (var col of filtered_column_names) {
		var col_dict = {
			title: col,
			field: col,
			headerVertical: "flip",
//			formatter: "tickCross",
//			formatterParams: {allowEmpty: true},
			width: 22,
			minWidth: 22,
			resizable: false,
			headerSort: false,
			cssClass: "smallish",


			tooltip: function(cell) {
				//cell - cell component


				var cell_value = cell.getValue();
				if (cell_value != null) {
					return cell_value["failure_mode"]["contents"]["line_text"];
				} else {
					return "";
				}



				//function should return a string for the tooltip of false to hide the tooltip
				return  cell.getColumn().getField() + " - " + cell.getValue(); //return cells "field - value";
			},


			formatter: function(cell, formatterParams, onRendered){
			    //cell - the cell component
			    //formatterParams - parameters set for the column
			    //onRendered - function to call when the formatter has been rendered



				var cell_value = cell.getValue();
				if (cell_value != null) {
					return '<img src="/images/red-x.svg" style="width: 100%; top: 50%;"/>';
				} else {
					return "";
				}

			},

			cellClick: function(e, cell) {
				//e - the click event object
				//cell - cell component
				var cell_value = cell.getValue()
				console.log("cell: " + cell_value + "; commit: " + cell.getRow().getData()["commit"]);

				if (cell_value != null) {
					var build_id = cell_value["build"]["build_id"];
					console.log("build: " + build_id);


/*

					var should_view = confirm("View build " + build_id + "?");
					if (should_view) {
						window.location.href = "/build-details.html?build_id=" + build_id;
					}
*/
				}
			},
			cellContext: function(e, cell) {

				var cell_value = cell.getValue()
				console.log("CONTEXT cell: " + cell_value + "; commit: " + cell.getRow().getData()["commit"]);


				//e - the click event object
				//cell - cell component



				if (cell_value != null) {
					var build_id = cell_value["build"]["build_id"];
					console.log("CONTEXT build: " + build_id);


					var cell_element = cell.getElement();

					var x = event.clientX;     // Get the horizontal coordinate
					var y = event.clientY;     // Get the vertical coordinate
					var coor = "X coords: " + x + ", Y coords: " + y;
					console.log(coor);

					var dropdown_element = document.getElementById("myDropdown");
					dropdown_element.style.left = event.pageX;
					dropdown_element.style.top = event.pageY;

					showContextMenu();


				}
				e.preventDefault();
			},

		};
		column_list.push(col_dict);
	}

	// global
	build_failures_by_commit = {};

	for (var failure_obj of fetched_data.failures) {
		var failures_by_job_name = setDefault(build_failures_by_commit, failure_obj.build.vcs_revision, {});
		failures_by_job_name[failure_obj.build.job_name] = failure_obj;
	}

	var table_data = [];
	for (var commit_obj of fetched_data.commits) {
		var row_dict = {};

		var sha1 = commit_obj.record;
		var failures_by_job_name = build_failures_by_commit[sha1] || {};

		row_dict["commit"] = sha1;

		for (var job_name in failures_by_job_name) {
			row_dict[job_name] = failures_by_job_name[job_name];
		}

		table_data.push(row_dict);
	}


	var table = new Tabulator("#" + element_id, {
		layout: "fitColumns",
		placeholder: "No Data Set",
		selectable: true,
		columns: column_list,
		data: table_data, //set initial table data
	});

}


function main() {
	get_timeline_data(0, 60);



	// Close the dropdown menu if the user clicks outside of it
	window.onclick = function(event) {
	  if (!event.target.matches('.dropbtn')) {
	    var dropdowns = document.getElementsByClassName("dropdown-content");
	    var i;
	    for (i = 0; i < dropdowns.length; i++) {
	      var openDropdown = dropdowns[i];
	      if (openDropdown.classList.contains('show')) {
		openDropdown.classList.remove('show');
	      }
	    }
	  }
	}

}




/* When the user clicks on the button, 
toggle between hiding and showing the dropdown content */
function showContextMenu() {
  document.getElementById("myDropdown").classList.toggle("show");
}

