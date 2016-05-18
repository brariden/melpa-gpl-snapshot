var ZeitgeistNewMailListener = {

	init: function() {
		var notificationService = Components.classes["@mozilla.org/messenger/msgnotificationservice;1"]
			.getService(Components.interfaces.nsIMsgFolderNotificationService);
		notificationService.addListener(this, notificationService.msgAdded); 
	},

	msgAdded: function(aMsgHdr) {

		let folder = aMsgHdr.folder;

		// Only add to zeitgeist if the folder the message is in is flagged as an inbox ("0x00001000")
		if (folder.flags & "0x00001000") { 
			
			let hdrParser = Components.classes["@mozilla.org/messenger/headerparser;1"]
							.getService(Components.interfaces.nsIMsgHeaderParser);

			let uri = folder.getUriForMsg(aMsgHdr);  
			let author = hdrParser.extractHeaderAddressName(aMsgHdr.mime2DecodedAuthor);
			let address = hdrParser.extractHeaderAddressMailboxes(aMsgHdr.author);
			let message_subject = aMsgHdr.mime2DecodedSubject
			let account = aMsgHdr.folder.server.prettyName

			let subject = libzeitgeist.zeitgeist_subject_new_full( uri,
										"http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#Email",
										"http://www.semanticdesktop.org/ontologies/2007/03/22/nmo#MailboxDataObject",
										"message/rfc822",
										address,
										author + " - " + message_subject,
										"net");

			let event = libzeitgeist.zeitgeist_event_new_full(  "http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#ReceiveEvent", 
												"http://www.zeitgeist-project.com/ontologies/2010/01/27/zg#SystemNotification", 
												"application://thunderbird.desktop",
												subject,
												null);

			libzeitgeist.zeitgeist_event_set_origin( event,
												account)

			libzeitgeist.zeitgeist_log_insert_events_no_reply(libzeitgeist.log, event, null);

			//Log event in Thunderbird's error console if logging pref is true
			var prefs = Components.classes["@mozilla.org/preferences-service;1"]
									.getService(Components.interfaces.nsIPrefBranch);
			if (prefs.getBoolPref("extensions.zeitgeist.log")) {
				zeitgeist.debug("Event added to zeitgeist:" +
								"\n\t\tEvent interpretation: EVENT_INTERPRETATION.RECEIVE_EVENT" +
								"\n\t\tEvent manifestation: EVENT_MANIFESTATION.SYSTEM_NOTIFICATION" +
								"\n\t\tActor: application://thunderbird.desktop" +
								"\n\t\tOrigin: " + account +
								"\n\t\tSubject:\n\t\t\tSubject interpretation: MESSAGE.EMAIL" +
								"\n\t\t\tSubject manifestation: MAILBOX_DATA_OBJECT" +
								"\n\t\t\turl: " + uri +
								"\n\t\t\tmimetype: message/rfc822" + 
								"\n\t\t\torigin: " + address + 
								"\n\t\t\ttitle: " + author + " - " + message_subject +
								"\n\t\t\tstorage: net");
			}
		}
	},
};

var zeitgeist = {
	init: function() { 
		ZeitgeistNewMailListener.init();
		libzeitgeist.init();
	},

	uninit: function() {

		libzeitgeist.shutdown();
	},

	debug: function (aMessage) {
		var consoleService = Components.classes["@mozilla.org/consoleservice;1"]
			.getService(Components.interfaces.nsIConsoleService);
		consoleService.logStringMessage("Zeitgeist Extension (" + new Date() + " ):\n\t" + aMessage);
		window.dump("Zeitgeist Extension: (" + new Date() + " ):\n\t" + aMessage + "\n");
	}
};

window.addEventListener("load", function() {zeitgeist.init()}, false);
window.addEventListener("unload", function() {zeitgeist.uninit()}, false);
