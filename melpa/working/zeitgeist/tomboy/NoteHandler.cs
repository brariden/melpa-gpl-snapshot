using System;
using Zeitgeist.Datamodel;
using System.Collections.Generic;

namespace Tomboy.Zeitgeist
{
	public class NoteHandler
	{
		public NoteHandler(Note note, ZeitgeistHandler handler)
		{
			// Check if the Note has already been processes.
			// Whent he note is being processed, event handlers are attached to
			if (handledNotes.Contains(note) == false)
			{
				this._note = note;
				this._handler =  handler;
				
				note.Opened += HandleNoteOpened;
				if (note.HasWindow)
				{
					HandleNoteOpened();
				}
			}
		}
		
		void HandleNoteOpened (object sender, EventArgs e)
		{
			HandleNoteOpened();
		}
		
		void HandleNoteOpened()
		{
			this._note.Window.Hidden += HandleNoteWindowHidden;
			this._note.Window.Shown += HandleNoteWindowShown;
			this._note.Renamed += HandleNoteRenamed;
			if (this._note.Window.Visible)
			{
				HandleNoteWindowShown();	
			}
		}
		
		void HandleNoteRenamed (Note sender, string old_title)
		{
			Console.WriteLine("Zg#: Renamed: " + this._note.Title);
			this._handler.SendEvent(sender, Interpretation.Instance.EventInterpretation.ModifyEvent);
		}
		
		void HandleNoteWindowShown (object sender, EventArgs e)
		{
			HandleNoteWindowShown();
		}
		
		void HandleNoteWindowShown ()
		{
			Console.WriteLine("Zg#: Note window opened: " + this._note.Title);
			this._handler.SendEvent(this._note, Interpretation.Instance.EventInterpretation.AccessEvent);
		}
		
		void HandleNoteWindowHidden (object sender, EventArgs e)
		{
			Console.WriteLine("Zg#: Note window closed: " + this._note.Title);
			this._handler.SendEvent(this._note, Interpretation.Instance.EventInterpretation.LeaveEvent);
		}
		
		private Note _note;
		
		private static List<Note> handledNotes = new List<Note>();
		
		private ZeitgeistHandler _handler;
	}
}

