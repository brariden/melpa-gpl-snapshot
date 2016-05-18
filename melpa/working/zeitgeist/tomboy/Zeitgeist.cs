using System;
using Zeitgeist.Datamodel;
using Zeitgeist;
using System.Collections.Generic;
using Tomboy;

namespace Tomboy.Zeitgeist
{
	public class ZeitgeistHandler
	{
		public ZeitgeistHandler()
		{
			client = new LogClient();
		}
		
        private LogClient client;
		
		public bool SendEvent(Note note, NameUri eventInterpretation)
		{
			NameUri eventManifestation = Manifestation.Instance.EventManifestation.UserActivity;
			
			NameUri subjectInterpretation = Interpretation.Instance.Document.Document;
			NameUri subjectManifestation = Manifestation.Instance.FileDataObject.FileDataObject;
			
			Event ev = EventCreator.CreateEvent(note, eventInterpretation, eventManifestation, subjectInterpretation, subjectManifestation);
			
			
			try
			{
				List<Event> listOfEvents = new List<Event>();
				listOfEvents.Add(ev);
				
				GLib.Idle.Add(delegate()
							{
								SendEvent(listOfEvents);
								return false;
							});
				
				Console.WriteLine(string.Format("Operation {0} successful", ev.Interpretation.Name));
				return true;
			}
			catch(Exception e)
			{
				Console.WriteLine(e.StackTrace);
				
				return false;
			}
		}
		
		private void SendEvent(List<Event> e)
		{
			client.InsertEvents(e);
		}
	}
	
	public class EventCreator
	{
		public static Event CreateEvent(Note note,
		                          NameUri eventInterpretation, 
		                          NameUri eventManifestation, 
		                          NameUri subjectInterpretation, 
		                          NameUri subjectManifestation)
		{
			Event ev = new Event();
			
			
			ev.Id = 0;
			ev.Timestamp = DateTime.Now;
			ev.Interpretation = eventInterpretation;
			ev.Manifestation = eventManifestation;
			ev.Actor = ZeitgeistAddin.TomboyUri;
			
			Subject sub = new Subject();
			
			sub.Uri = note.Uri;
			sub.Interpretation = subjectInterpretation;
			sub.Manifestation = subjectManifestation;
			sub.Origin = string.Empty;
			sub.MimeType = ZeitgeistAddin.NoteMimetype;
			sub.Text = note.Title;
			sub.Storage = string.Empty;
			
			ev.Subjects.Add(sub);
			
			return ev;
		}
	}
}

