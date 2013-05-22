
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

// Display links for a single peer
function peer_links(peer)
{
    switch (peer.type) {
        case 'ip':
            // Instead of using peer.label, use a dedicated peer.value for forms?
            return (
                '<a href="?action=Traffic%2FCall%20Flow&filter%2Fip-start=' + encodeURI(peer.label) + '">Callflow</a>' +
                '<a href="?action=Traffic%2FBandwidth%20Evolution%2Fshow&filter%2Fip=' + encodeURI(peer.label) + '&filter%2Ftraffic-groupby=0&filter%2Fvol-or-count=0">Bandwidth</a>'
                // TODO: add DNS/HTTP queries to/from
            );
        case 'mac':
            return (
                '<a href="?action=Traffic%2FBandwidth%20Evolution%2Fshow&filter%2Ftraffic-groupby=0&filter%2Fvol-or-count=0&filter%2Feth-src=' + encodeURI(peer.label) + '">Bandwidth from</a>' +
                '<a href="?action=Traffic%2FBandwidth%20Evolution%2Fshow&filter%2Ftraffic-groupby=0&filter%2Fvol-or-count=0&filter%2Feth-dest=' + encodeURI(peer.label) + '">Bandwidth to</a>'
            );
        default:
            return '';
    }
}

function set_peer_links(peer)
{
    var html = peer_links(peer);
    document.getElementById('selected-peer-links').innerHTML = html;
}

function link_links(peer1, peer2)
{
    document.getElementById('selected-peer-links').innerHTML =
        '<a href="?action=Traffic%2FCall%20Flow&filter%2Fip-start=' + encodeURI(peer1) + '">Callflow '+peer1+'</a>' +
        '<a href="?action=Traffic%2FCall%20Flow&filter%2Fip-start=' + encodeURI(peer2) + '">Callflow '+peer2+'</a>' +
        '<a href="?action=Traffic%2FBandwidth%20Evolution%2Fshow&filter%2Fip-src=' + encodeURI(peer1) + '&filter%2Fip-dst=' + encodeURI(peer2) + '&filter%2Ftraffic-groupby=0&filter%2Fvol-or-count=0">Bandwidth '+ peer1 +'&rarr;'+ peer2 +'</a>' +
        '<a href="?action=Traffic%2FBandwidth%20Evolution%2Fshow&filter%2Fip-src=' + encodeURI(peer2) + '&filter%2Fip-dst=' + encodeURI(peer1) + '&filter%2Ftraffic-groupby=0&filter%2Fvol-or-count=0">Bandwidth '+ peer2 +'&rarr;'+ peer1 +'</a>';
}

function label_select(peer, infos)
{
    var all = document.getElementsByClassName(peer.label);
    for (var i = 0; i < all.length; i++) {
        all[i].classList.add('selected');
    }
    // fill in infos for this peer
    document.getElementById('selected-peer-name').innerHTML = peer.label;
    document.getElementById('selected-peer-info').innerHTML = infos;
    set_peer_links(peer);
}

function label_unselect(peer)
{
    var all = document.getElementsByClassName(peer.label);
    for (var i = 0; i < all.length; i++) {
        all[i].classList.remove('selected');
    }
}

function link_select(classname, peer1, peer2)
{
    document.getElementById('selected-peer-name').innerHTML = peer1.label +'&harr;'+ peer2.label;
    document.getElementById('selected-peer-info').innerHTML = '';
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
    var classes = elmt.getAttribute("class");
    return class_name.length <= classes.length && (
			classes.indexOf(' ' + class_name + ' ') > -1 || // found in the middle
			classes.indexOf(class_name + ' ') == 0 ||       // in the beginning
			classes.indexOf(class_name) == classes.length - class_name.length); // or in the end (or if the only class)
}

// FIXME: should take a label (ie. callflow should return labels as well)
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
    set_peer_links(peer_name);  // FIXME
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

function svg_explore_plot(svg_id, vx_min, vx_max, x_axis_xmin, x_axis_xmax, prec, start_field, stop_field, prec_field)
{
    var svg = document.getElementById(svg_id);
    var root = document.documentElement;
    var rect = svg.getBoundingClientRect();
    var cursor = document.getElementById("cursor");
    var drag_start_x = 0.;
    var dragging = false;

    function x_to_vx(x)
    {
        return vx_min + ((x - x_axis_xmin) * (vx_max-vx_min)) / (x_axis_xmax-x_axis_xmin);
    }

    function do_zoom(zoom_start, zoom_stop)
    {
        var start_v = x_to_vx(zoom_start);
        var stop_v  = x_to_vx(zoom_stop);
        var prec_v  = prec * (zoom_stop-zoom_start)/(x_axis_xmax-x_axis_xmin);
        window.location.search +=
            '&' + encodeURIComponent(start_field) +
                '=' + encodeURIComponent(start_v) +
            '&' + encodeURIComponent(stop_field) +
                '=' + encodeURIComponent(stop_v) +
            '&' + encodeURIComponent(prec_field) +
                '=' + encodeURIComponent(prec_v)
    }

    function get_svg_x(e)
    {
        return e.clientX - rect.left - root.scrollLeft;
    }

    // horizontal selection
    svg.addEventListener("mousedown", function (e) {
        e.preventDefault();
        drag_start_x = get_svg_x(e);
        dragging = true;
        cursor.setAttribute("x", drag_start_x);
        cursor.setAttribute("width", 1.);
        return false;
    }, false);

    svg.addEventListener("mousemove", function (e) {
        e.preventDefault();
        if (dragging) {
            var stop_x = get_svg_x(e);
            var width = stop_x - drag_start_x;
            if (width >= 0) {
                cursor.setAttribute("width", width);
            } else {
                cursor.setAttribute("x", stop_x);
                cursor.setAttribute("width", -width);
            }
        }
        return false;
    }, false);

    svg.addEventListener("mouseup", function (e) {
        e.preventDefault();
        if (dragging) {
            dragging = false;
            var zoom_start = parseFloat(cursor.getAttribute("x"));
            var zoom_stop = zoom_start + parseFloat(cursor.getAttribute("width"));
//            if (zoom_start < x_axis_xmin) zoom_start = x_axis_xmin;
//            if (zoom_stop < x_axis_xmin) zoom_stop = x_axis_xmin;
            if (zoom_stop - zoom_start >= 20) {
                do_zoom(zoom_start, zoom_stop);
            } else {
                cursor.setAttribute("width", 0);
            }
        }
        return false;
    }, false);
}


// Functions for time plot
/*
function plot_select2(peer, info)
{
    // Make svg elements more visible
    var all = document.getElementsByClassName("fitem");
    for (var i = 0; i < all.length; i++) {
        all[i].setAttribute("opacity",
            has_class(all[i], peer.label) ? 1 : 0.1);
    }
    // Make html elements more visible
    all = document.getElementsByClassName("hitem");
    for (var i = 0; i < all.length; i++) {
        if (has_class(all[i], peer.label)) {
            all[i].className += " selected";
        }
    }

    plot_select(peer, info);
}

function plot_unselect2(peer)
{
    var all = document.getElementsByClassName("fitem");
    for (var i = 0; i < all.length; i++) {
        all[i].setAttribute("opacity", 1);
    }
    all = document.getElementsByClassName("hitem");
    for (var i = 0; i < all.length; i++) {
        all[i].className = all[i].className.replace(/\bselected\b/,'');
    }
}

// These are called from the SVG, with the event
function plot_select(evt, peer, info)
{
    plot_select2(peer, info);
}

function plot_unselect(evt, peer)
{
    plot_unselect2(peer);
}
*/
