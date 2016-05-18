using System;
using Tomboy;
using System.Collections.Generic;
using System.IO;
using Zeitgeist.Datamodel;
using Zeitgeist;

namespace Tomboy.Zeitgeist
{
	public class ZeitgeistAddin  : ApplicationAddin
	{
		public ZeitgeistAddin ()
		{
			notesList = new List<NoteHandler>();
			
			dsReg = new DataSourceClient();
			handler = new ZeitgeistHandler();
		}

		
		#region Overridden methods
		
		public override bool Initialized
		{
			get
			{
				return _init;
			}
		}
		
		public override void Initialize()
		{
			Console.WriteLine("Zg#: init new");
			
			Event ev = new Event();
			ev.Actor = ZeitgeistAddin.TomboyUri;
			Subject sub = new Subject();
			sub.Interpretation = Interpretation.Instance.Document.Document;
			sub.Manifestation = Manifestation.Instance.FileDataObject.FileDataObject;
			ev.Subjects.Add(sub);
			
			try
			{
				dsReg.RegisterDataSources(tomboyDataSourceId, 
			                          	tomboyDataSourceName, 
			                          	tomboyDataSourceDesc , 
			                          	new List<Event>(){ev});
			}
			catch(Exception)
			{}
			
			// Initialize the handlers for hooking into Tomboy
			InitHandlers();
			
			_init = true;
		}
		
		public override void Shutdown()
		{
			Console.WriteLine("Zg#: shutdown");
		}
		
		#endregion
		
		public void InitHandlers()
		{
			// For every note present in the store
			
			foreach (Note note in Tomboy.DefaultNoteManager.Notes)
			{
				notesList.Add(new NoteHandler(note, handler));
			}
			
			Tomboy.DefaultNoteManager.NoteAdded -= HandleNoteAdded;
			Tomboy.DefaultNoteManager.NoteAdded += HandleNoteAdded;
			
			Tomboy.DefaultNoteManager.NoteDeleted -= HandleNoteDeleted;
			Tomboy.DefaultNoteManager.NoteDeleted += HandleNoteDeleted;
		}
		
		void HandleNoteAdded(object sender, Note new_note)
		{
			Console.WriteLine("Zg#: Note added: " + new_note.Title);
			Console.WriteLine("\t" + new_note.Uri);
			
			notesList.Add(new NoteHandler(new_note, handler));
			
			handler.SendEvent(new_note, Interpretation.Instance.EventInterpretation.CreateEvent);
		}
		
		void HandleNoteDeleted(object sender, Note new_note)
		{
			Console.WriteLine("Zg#: Note deleted: " + new_note.Title);
			Console.WriteLine("\t" + new_note.Uri);
			
			handler.SendEvent(new_note, Interpretation.Instance.EventInterpretation.DeleteEvent);
		}
		
		List<NoteHandler> notesList;
		
		private bool _init = false;
		
		private DataSourceClient dsReg; 
		
		private ZeitgeistHandler handler;
		
		#region Public Constants
				
		public const string TomboyUri = "application://tomboy.desktop";
		
		public const string NoteMimetype = "application/x-note";
		
		#endregion
		
		#region Private Constants
		
		private const string tomboyDataSourceId = "org.gnome.Tomboy,dataprovider";
		
		private const string tomboyDataSourceName = "Tomboy Datasource";
		
		private const string tomboyDataSourceDesc = "This datasource pushes the 4 events for tomboy - Open/Close Note, Create/Delete Note";
		
		#endregion
	}
}

