function zgGetContentTypeFromHeader() {
	var nodes = document.getElementsByTagName("meta");
	for (var i=0; i<nodes.length; i++)
	{
		var node = nodes[i];
		if (!node.hasAttributes()) continue;
		var http_equiv = node.getAttribute("http-equiv");
		if (http_equiv && http_equiv.toLowerCase() == "content-type")
		{
			var content_type = node.getAttribute("content");
			if (!content_type) continue;
			return content_type.split(';')[0];
		}
	}
	return null;
}

function zgGetDocumentInfo () {
	var docInfo = {
		"url": document.URL,
		"origin": document.referrer,
		"title": document.title
	};

	if (document.domain) {
		docInfo["domain"] = document.location.protocol + "//" + document.domain;
	}

	var contentType = zgGetContentTypeFromHeader();
	if (contentType) {
		docInfo["mimeType"] = contentType;
		chrome.extension.sendRequest(docInfo);
	} else {
		// send extra request to get the mime type
		var request = new XMLHttpRequest();
		request.open("HEAD", document.URL, true);
		request.onreadystatechange=function() {
			if (request.readyState==4) {
				var content = request.getResponseHeader("Content-Type");
				if (!content) return;
				docInfo["mimeType"] = content.split(';')[0];
				chrome.extension.sendRequest(docInfo);
			}
		}
		request.send(null);
	}
}

/*
function zgReadyStateChanged () {
	console.log("ready state change fired! " + document.readyState);
	if (document.readyState == "complete") {
		zgGetDocumentInfo();
		document.removeEventListener("readystatechange", zgReadyStateChanged, false);
	}
}*/

function zgTimeoutElapsed () {
	if (document.readyState == "complete") {
		zgGetDocumentInfo();
	} else {
		window.setTimeout(zgTimeoutElapsed, 1000);
	}
}

if (document.readyState != "complete") {
	// in a perfect world this would work
	//document.addEventListener("readystatechange", zgReadyStateChanged, false);
	// in real world we have to be nasty
	window.setTimeout(zgTimeoutElapsed, 1000);
} else {
	zgGetDocumentInfo();
}
