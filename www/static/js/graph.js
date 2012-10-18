
function graph_init() {
	google.load('visualization', '1.0', {'packages':['corechart','table']});
//	google.setOnLoadCallback(drawChart);
}

function http_request(url, handler) {
	if (window.XMLHttpRequest) {
		// code for IE7+, Firefox, Chrome, Opera, Safari
		xmlhttp = new XMLHttpRequest();
	} else {
	    // code for IE6, IE5
		xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
	}

	xmlhttp.onreadystatechange = function() {
		if (xmlhttp.readyState==4 && xmlhttp.status==200) {
			handler(xmlhttp.responseText);
		}
	}
	xmlhttp.open("GET", url, true);
	xmlhttp.send();
}

function refresh_chart(form) {
	var params = "";
	for (var i = 0; i < form.length; i++) {
		params = params + "&" + form.elements[i].name + "=" + form.elements[i].value;
	}

	http_request("?"+params, function (txt) {
		var data = new google.visualization.DataTable(eval(txt));
	    var options = {'title':'Test data'};
		var chart = new google.visualization.LineChart(document.getElementById('chart_div'));
	    chart.draw(data, options);
	});

/*
    var data = new google.visualization.DataTable();
    data.addColumn('string', 'Topping');
    data.addColumn('number', 'Slices');
    data.addRows([
      ['Mushrooms', 3],
      ['Onions', 1],
      ['Olives', 1],
      ['Zucchini', 1],
      ['Pepperoni', 2]
    ]);

    // Set chart options
    var options = {'title':'How Much Pizza I Ate Last Night',
                   'width':400,
                   'height':300};

    // Instantiate and draw our chart, passing in some options.
    var chart = new google.visualization.PieChart(document.getElementById('chart_div'));
    chart.draw(data, options);
*/
}

