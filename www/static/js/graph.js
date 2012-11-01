
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

}

/* function for SVG circle-graph */

function peer_select(evt, peer_name)
{
    var classname = evt.target.getAttribute("class");
    var all = document.getElementsByClassName(classname);
    for (var i = 0; i < all.length; i++) {
        var col = all[i].getAttribute("fill");
        all[i].setAttribute("fill", "rgb(255,150,5)");
    }
}

function peer_unselect(evt, peer_name)
{
    var classname = evt.target.getAttribute("class");
    var all = document.getElementsByClassName(classname);
    for (var i = 0; i < all.length; i++) {
        var col = all[i].getAttribute("stroke");
        all[i].setAttribute("fill", col);
    }
}
