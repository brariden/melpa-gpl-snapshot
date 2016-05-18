/*
 * zeitgeist-dataprovider.c
 * This file is part of Zeitgeist dataprovider for XChat.
 *
 * Copyright (C) 2011 - Stefano Candori <stefano.candori@gmail.com>
 *
 * Zeitgeist dataprovider for XChat is free software; 
 * you can redistribute it and/or modify it under the terms of the GNU Lesser 
 * General Public License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * Zeitgeist dataprovider for XChat is distributed in the hope that
 * it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

#include "xchat-plugin.h"
#include <zeitgeist.h>

#define PNAME "Zeitgeist"
#define PDESC "Inform Zeitgeist about your activity"
#define PVERSION "0.1"

static xchat_plugin    *ph           = NULL;   /* plugin handle */
static ZeitgeistLog    *zg_log       = NULL;   /* Zeitgeist-daemon hanlde*/
static GSList          *channel_list = NULL;   /* List of joined channels */

static void send_event_to_zeitgeist(char *url_, char* text_, const char* ev_interpretation)
{
  const char *url, *origin, *mimetype, *text;
  const char *interpretation = NULL;
  const char *manifestation = NULL;
  ZeitgeistEvent *event;
  
  url = url_;
  origin = url;
  mimetype = "text/plain";
  text = text_;
  interpretation = ZEITGEIST_NMO_IMMESSAGE;
  manifestation = ZEITGEIST_NFO_SOFTWARE_SERVICE;
           
  event = zeitgeist_event_new_full (
      ev_interpretation,
      ZEITGEIST_ZG_USER_ACTIVITY,
      "application://xchat.desktop",
      zeitgeist_subject_new_full (
        url,
        interpretation,
        manifestation,
        mimetype,
        origin,
        text,
        "net"),
  NULL);

  /*g_debug ("URL: %s, origin: %s, mimeType: %s, text: %s, interpretation: %s",
           url,
           origin,
           mimetype,
           text,
           ev_interpretation);*/
           
  zeitgeist_log_insert_events_no_reply (zg_log, event, NULL);
}

static int join_cb(char *word[], void *userdata)
{
   const char *server = xchat_get_info(ph, "host");
   const char *channel = word[2];
   char *url, *text;
   
   channel_list = g_slist_prepend(channel_list, g_strdup(channel));
   
   url = g_strconcat("irc://", server, "/", channel, NULL);
   text = g_strconcat("You joined ", channel, NULL);
   
   send_event_to_zeitgeist(url, text, ZEITGEIST_ZG_ACCESS_EVENT);
   
   g_free(url);
   g_free(text);
   
   return XCHAT_EAT_NONE;
}

static int part_cb(char *word[], char* word_eol[], void *userdata)
{
   const char *server = xchat_get_info(ph, "host");
   const char *channel = word[3];
   char *url, *text;
   GSList *tmp = channel_list;
  
   url = g_strconcat("irc://", server, "/", channel, NULL);  
   text = g_strconcat("You parted from ", channel, NULL);
   
   send_event_to_zeitgeist(url, text, ZEITGEIST_ZG_LEAVE_EVENT);
   
   while(tmp)
   {
      if (g_strcmp0 (tmp->data, channel) == 0)
      {
         g_free(tmp->data);
         channel_list = g_slist_delete_link(channel_list, tmp);
         break;
      }     
      tmp = tmp->next;
   }
   
   g_free(url);
   g_free(text);

   return XCHAT_EAT_NONE;
}

/*static int message_cb(char *word[], void *userdata)
{
   const char *server = xchat_get_info(ph, "host");
   const char *channel = xchat_get_info(ph, "channel");
   char *url, *text;
   
   url = g_strconcat("irc://", server, "/", channel, NULL);  
   text = g_strconcat("IRC ", word[2],"\".", NULL);
   
   send_event_to_zeitgeist(url, text, ZEITGEIST_ZG_SEND_EVENT);

   g_free(url);
   g_free(text);
   
   return XCHAT_EAT_NONE;
}*/

/*static int priv_message_cb(char *word[], void *userdata)
{
   const char *server = xchat_get_info(ph, "host");
   const char *channel = xchat_get_info(ph, "channel");
   char *url, *text;
   
   url = g_strconcat("irc://", server, "/", channel, NULL);  
   text = g_strconcat("IRC ", word[1], NULL);
   
   send_event_to_zeitgeist(url, text, ZEITGEIST_ZG_RECEIVE_EVENT);
   
   g_free(url);
   g_free(text);
   
   return XCHAT_EAT_NONE;
}*/

static void on_quit(gpointer data, gpointer userdata)
{
   const char *server = xchat_get_info(ph, "host");
   const char *channel = (const char*) data;
   char *url, *text;   
   
   url = g_strconcat("irc://", server, "/", channel, NULL);  
   text = g_strconcat("You parted from ", channel, NULL);
   
   send_event_to_zeitgeist(url, text, ZEITGEIST_ZG_LEAVE_EVENT);
   
   g_free(url);
   g_free(text);
}

void xchat_plugin_get_info(char **name, char **desc, char **version, void **reserved)
{
   *name = PNAME;
   *desc = PDESC;
   *version = PVERSION;
}

static int initialized = 0;
static int reinit_tried = 0;

int xchat_plugin_init(xchat_plugin *plugin_handle,
                      char **plugin_name,
                      char **plugin_desc,
                      char **plugin_version,
                      char *arg)
{
   ph = plugin_handle;
   
   /* Block double initalization. */
   if (initialized != 0) {
      xchat_print(ph, "Zeitgeist plugin already loaded");
      /* deinit is called even when init fails, so keep track
       * of a reinit failure. */
      reinit_tried++;
      return 0;
   }
   initialized = 1;

   *plugin_name = PNAME;
   *plugin_desc = PDESC;
   *plugin_version = PVERSION;
   
   zg_log = zeitgeist_log_new();

   xchat_hook_print(ph, "You Join", XCHAT_PRI_NORM, join_cb, 0);
   /*xchat_hook_print(ph, "Your Message", XCHAT_PRI_NORM, message_cb, 0);
   xchat_hook_print(ph, "Channel Msg Hilight", XCHAT_PRI_NORM, priv_message_cb, 0); */
   xchat_hook_server(ph, "PART", XCHAT_PRI_NORM, part_cb, 0);

   xchat_print(ph, "Zeitgeist plugin loaded\n");

   return 1;   
}

int xchat_plugin_deinit()
{
   /* A reinitialization was tried. Just give up */
   if (reinit_tried) {
      reinit_tried--;
      return 1;
   }
   
   g_slist_foreach(channel_list, on_quit, NULL);
   g_slist_foreach(channel_list, (GFunc)g_free, NULL);
   g_slist_free(channel_list);
   channel_list = NULL;
   
   g_object_unref(zg_log);
   zg_log = NULL;
   return 1;
}
