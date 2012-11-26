
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

/* Functions to display links about peer / link */

function peer_links(peer_name)
{
    document.getElementById('selected-peer-links').innerHTML =
        '<a href="?action=Traffic%2Fcallflow&filter.ip-start=' + encodeURI(peer_name) + '">Callflow</a>' +
        '<a href="?action=Traffic%2Fbandwidth&filter.ip=' + encodeURI(peer_name) + '">Bandwidth</a>';
}

function link_links(peer1, peer2)
{
    document.getElementById('selected-peer-links').innerHTML =
        '<a href="?action=Traffic%2Fcallflow&filter.ip-start=' + encodeURI(peer1) + '">Callflow '+peer1+'</a>' +
        '<a href="?action=Traffic%2Fcallflow&filter.ip-start=' + encodeURI(peer2) + '">Callflow '+peer2+'</a>' +
        '<a href="?action=Traffic%2Fbandwidth&filter.ip-src=' + encodeURI(peer1) + '&filter.ip-dst=' + encodeURI(peer2) + '">Bandwidth '+ peer1 +'&rarr;'+ peer2 +'</a>' +
        '<a href="?action=Traffic%2Fbandwidth&filter.ip-src=' + encodeURI(peer2) + '&filter.ip-dst=' + encodeURI(peer1) + '">Bandwidth '+ peer2 +'&rarr;'+ peer1 +'</a>';
}

/* function for SVG circle-graph */

function peer_select(evt, peer_name, up, down)
{
    var classname = evt.target.getAttribute("id");
    var all = document.getElementsByClassName(classname);
    for (var i = 0; i < all.length; i++) {
        all[i].setAttribute("fill", "rgb(255,150,5)");
        all[i].setAttribute("fill-opacity", 1);
    }
    // fill in infos for this peer
    document.getElementById('selected-peer-name').innerHTML = peer_name;
    document.getElementById('selected-peer-info').innerHTML =
        'up:&nbsp;'+up+'</br>'+
        'down:&nbsp;'+down;
    peer_links(peer_name);
}

function peer_unselect(evt)
{
    var classname = evt.target.getAttribute("id");
    var all = document.getElementsByClassName(classname);
    for (var i = 0; i < all.length; i++) {
        var col = all[i].getAttribute("stroke");
        all[i].setAttribute("fill", col);
        var opac = all[i].getAttribute("stroke-opacity");
        all[i].setAttribute("fill-opacity", opac);
    }
}

/* Functions to explore the netgraph */

function node_select(evt, peer_name)
{
    document.getElementById('selected-peer-name').innerHTML = peer_name;
    document.getElementById('selected-peer-info').innerHTML = '??';
    peer_links(peer_name);
}

function edge_select(evt, peer1, peer2)
{
    document.getElementById('selected-peer-name').innerHTML = peer1 +'&harr;'+ peer2;
    document.getElementById('selected-peer-info').innerHTML = '??';
    link_links(peer1, peer2);
}

/* Function to navigate svg graphs */

// Given the id of some svg:g, make it zoomable via the mouse
function svg_explorer(svg_id, controler_id)
{
    var zoom = 1;
    var trans_x = 0;
    var trans_y = 0;
    var g = document.getElementById(controler_id);
    var svg = document.getElementById(svg_id);

    function reset_transform() {
        g.setAttribute("transform", "scale("+zoom+") translate("+trans_x+" "+trans_y+")");
    }

    // zooming
	function zoom_handler(e) {
        e.preventDefault();
        var dir = Math.max(-1, Math.min(1, (e.wheelDelta || -e.detail)));

        var prev_zoom = zoom;
        zoom *= 1 + dir*0.3;
        if (zoom < 0.03) zoom = 0.03;

        var rect = svg.getBoundingClientRect(), root = document.documentElement;
        var cursor_x = e.clientX - rect.left - root.scrollLeft;
        var cursor_y = e.clientY - rect.top - root.scrollTop;
        var cursor_dx = cursor_x/zoom - cursor_x/prev_zoom;
        var cursor_dy = cursor_y/zoom - cursor_y/prev_zoom;
        trans_x += cursor_dx;
        trans_y += cursor_dy;

        reset_transform();
        return false;
	}
    svg.addEventListener("DOMMouseScroll", zoom_handler, false);
    svg.addEventListener("mousewheel", zoom_handler, false);

    // panning
    var dragging = false;
    var drag_start_x;
    var drag_start_y;
    var trans_x_start;
    var trans_y_start;
    svg.addEventListener("mousedown", function (e) {
        e.preventDefault();
        if (dragging) {
            // hum, really?
        } else {
            dragging = true;
            drag_start_x = e.clientX;
            drag_start_y = e.clientY;
            trans_x_start = trans_x;
            trans_y_start = trans_y;
        }
        return false;
    }, false);
    svg.addEventListener("mouseup", function (e) {
        e.preventDefault();
        if (dragging) {
            dragging = false;
        } else {
            // hu?
        }
        return false;
    }, false);
    svg.addEventListener("mousemove", function (e) {
        e.preventDefault();
        if (dragging) {
            var dx = e.clientX - drag_start_x;
            var dy = e.clientY - drag_start_y;
            trans_x = trans_x_start + dx/zoom; trans_y = trans_y_start + dy/zoom;
            reset_transform();
        }
        return false;
    }, false);
}

/* function for callflow graph */

function has_class(elmt, class_name)
{
    return elmt.getAttribute("class").indexOf(class_name) > -1;
}

function timeline_select(evt, peer_name)
{
    var all = document.getElementsByClassName("fitem");
    for (var i = 0; i < all.length; i++) {
        all[i].setAttribute("opacity",
            has_class(all[i], peer_name) ? 1 : 0.01);
    }
    // fill in infos for this peer
    document.getElementById('selected-peer-name').innerHTML = peer_name;
    document.getElementById('selected-peer-info').innerHTML = '??';
    peer_links(peer_name);
}

function timeline_unselect(evt, peer_name)
{
    var all = document.getElementsByClassName("fitem");
    for (var i = 0; i < all.length; i++) {
        all[i].setAttribute("opacity", 1);
    }
}

function dt_select(evt, peer1, peer2)
{
    document.getElementById('selected-peer-name').innerHTML = peer1 +'&harr;'+ peer2;
    document.getElementById('selected-peer-info').innerHTML = '??';
    link_links(peer1, peer2);
}

function tx_select(evt, peer1, peer2)
{
    document.getElementById('selected-peer-name').innerHTML = peer1 +'&harr;'+ peer2;
    document.getElementById('selected-peer-info').innerHTML = '??';
    link_links(peer1, peer2);
}
