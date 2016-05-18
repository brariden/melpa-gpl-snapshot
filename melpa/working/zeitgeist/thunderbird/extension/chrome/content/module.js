var libzeitgeist = {

	zeitgeistPath: "libzeitgeist-1.0.so.1",

	lib: null,

	init: function() {

		Components.utils.import("resource://gre/modules/ctypes.jsm");

		this.lib = ctypes.open(this.zeitgeistPath);

		//Structures
		this._ZeitgeistEvent = new ctypes.StructType("_ZeitgeistEvent");
		this._ZeitgeistSubject = new ctypes.StructType("_ZeitgeistSubject");
		this._ZeitgeistLog = new ctypes.StructType("_ZeitgeistLog");

		//Methods
		this.zeitgeist_event_new_full = this.lib.declare( "zeitgeist_event_new_full",
														ctypes.default_abi,
														ctypes.char.ptr,
														ctypes.char.ptr,	//interpretation
														ctypes.char.ptr,	//manifestation
														ctypes.char.ptr,	//actor
														ctypes.char.ptr,	//subject
														ctypes.voidptr_t);

		this.zeitgeist_event_set_origin = this.lib.declare( "zeitgeist_event_set_origin",
														ctypes.default_abi,
														ctypes.voidptr_t,
														ctypes.char.ptr,	//event
														ctypes.char.ptr);	//origin

		this.zeitgeist_subject_new_full = this.lib.declare( "zeitgeist_subject_new_full",
														ctypes.default_abi,
														ctypes.char.ptr,
														ctypes.char.ptr,	//uri
														ctypes.char.ptr,	//interpretation
														ctypes.char.ptr,	//manfestation
														ctypes.char.ptr,	//mimetype
														ctypes.char.ptr,	//origin
														ctypes.char.ptr,	//text
														ctypes.char.ptr);	//storage

		this.zeitgeist_log_new = this.lib.declare( "zeitgeist_log_new",
														ctypes.default_abi,
														ctypes.char.ptr	);

		this.zeitgeist_log_insert_events_no_reply = this.lib.declare( "zeitgeist_log_insert_events_no_reply",
														ctypes.default_abi,
														ctypes.void_t.ptr,
														ctypes.char.ptr,	//log
														ctypes.char.ptr,	//event
														ctypes.voidptr_t);                     

		this.log = libzeitgeist.zeitgeist_log_new();
	},

	shutdown: function() {
		this.lib.close();
	}
};
