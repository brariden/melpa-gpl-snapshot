/*
 * zeitgeist_dataprovider.c
 * This file is part of 'the zeitgeist plugin for geany' 
 *
 * Copyright (C) 2010 - Markus Korn <thekorn@gmx.de>
 *
 * 'the zeitgeist plugin for geany' is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * 'the zeitgeist plugin for geany' is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */



#include "geanyplugin.h"
#include <zeitgeist.h>

PLUGIN_VERSION_CHECK(147)

PLUGIN_SET_INFO("ZeitgeistDataprovider", "Dataprovider for zeitgeist",
				"0.1", "Markus Korn <thekorn@gmx.de>");

static ZeitgeistLog		*log;

static void insert_zeitgeist(GeanyDocument *doc,const char *action)
{
	char				*uri;
	gchar				*filetype;
	ZeitgeistEvent		*event;

	uri = DOC_FILENAME(doc);
	filetype = doc->file_type->name;

	event = zeitgeist_event_new_full (
		action,
		ZEITGEIST_ZG_USER_ACTIVITY,
		"app://geany.desktop",
		zeitgeist_subject_new_full (
			uri,
			ZEITGEIST_NFO_TEXT_DOCUMENT,
			ZEITGEIST_NFO_FILE_DATA_OBJECT,
			filetype,
			uri,
			uri,
			"net"),
		NULL);
		
	g_debug("inserting event");
	zeitgeist_log_insert_events_no_reply(log, event, NULL);
	g_debug("zeitgeist end");
}

static void on_document_open(GObject *obj, GeanyDocument *doc, gpointer user_data)
{
	g_debug("Example: %s was opened\n", DOC_FILENAME(doc));
	insert_zeitgeist(doc, ZEITGEIST_ZG_ACCESS_EVENT);
}

static void on_document_close(GObject *obj, GeanyDocument *doc, gpointer user_data)
{
	g_debug("Example: %s was closed\n", DOC_FILENAME(doc));
	insert_zeitgeist(doc, ZEITGEIST_ZG_LEAVE_EVENT);
}

static void on_document_new(GObject *obj, GeanyDocument *doc, gpointer user_data)
{
	g_debug("Example: %s was created\n", DOC_FILENAME(doc));
	insert_zeitgeist(doc, ZEITGEIST_ZG_CREATE_EVENT);
}

static void on_document_activate(GObject *obj, GeanyDocument *doc, gpointer user_data)
{
	g_debug("Example: %s was opened\n", DOC_FILENAME(doc));
	insert_zeitgeist(doc, ZEITGEIST_ZG_ACCESS_EVENT);
}

PluginCallback plugin_callbacks[] =
{
	{ "document-open", (GCallback) &on_document_open, FALSE, NULL },
	{ "document-close", (GCallback) &on_document_close, FALSE, NULL },
	{ "document-new", (GCallback) &on_document_new, FALSE, NULL },
	{ "document-activate", (GCallback) &on_document_activate, FALSE, NULL },
	{ NULL, NULL, FALSE, NULL }
};

void plugin_init(GeanyData *data)
{
	log = g_object_new (ZEITGEIST_TYPE_LOG, NULL);
	g_type_init ();
	
}

void plugin_cleanup(void)
{}
