//
//  ZeitgeistClient.cs
//
//  Author:
//       Patrick McEvoy <patrick@qmtech.net>
//	 Manish Sinha <manishsinha@ubuntu.com>
//
//  Copyright (c) 2011 QMTech.
//  Copyright (c) 2011 Manish Sinha <manishsinha@ubuntu.com>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify,
// merge, publish, distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
// OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
// CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
// SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

using System;

using MonoDevelop.Ide.Gui;

using Zeitgeist.Datamodel;
using Zeitgeist;
using System.Collections.Generic;
using MonoDevelop.Ide;
using MonoDevelop.Projects;
using MonoDevelop.Core;

namespace MonoDevelop.Zeitgeist
{
	internal enum EventType {
		Access,
		Modify,
		Leave,
		Move,
		Create,
		Delete,
	}

	internal class ZeitgeistClient
	{
		public const string MonoDevelopUri = "application://monodevelop.desktop";
		public const string Mimetype = "text/plain";

		const string monodevelopDataSourceId = "org.gnome.MonoDevelop,dataprovider";
		const string monodevelopDataSourceName = "MonoDevelop Datasource";
		const string monodevelopDataSourceDesc = "This datasource pushes the file open, delete, modify events for files and directories edited in MonoDevelop";

		DataSourceClient dsReg;
		LogClient client;

		public ZeitgeistClient ()
		{
			dsReg = new DataSourceClient ();

			MonoDevelop.Core.LoggingService.LogInfo ("Zg#: init new");

			Event ev = new Event ();
			ev.Actor = MonoDevelopUri;
			Subject sub = new Subject ();
			sub.Interpretation = Interpretation.Instance.Document.Document;
			sub.Manifestation = Manifestation.Instance.FileDataObject.FileDataObject;
			ev.Subjects.Add (sub);

			try {
				dsReg.RegisterDataSources (monodevelopDataSourceId,
					monodevelopDataSourceName,
					monodevelopDataSourceDesc,
					new List<Event> (){ev});
				client = new LogClient ();
			} catch (Exception ex) {
				MonoDevelop.Core.LoggingService.LogError ("Error init", ex);
			}
		}
		
		public void SendFilePath (FilePath oldPath, FilePath newPath, EventType type)
		{
			var eventManifestation = Manifestation.Instance.EventManifestation.UserActivity;
			var subjectInterpretation = Interpretation.Instance.Document.TextDocument.PlainTextDocument.SourceCode;
			var subjectManifestation = Manifestation.Instance.FileDataObject.FileDataObject;

			var interpretation = Interpretation.Instance.EventInterpretation.ModifyEvent;
			switch (type) {
			case EventType.Access:
				interpretation = Interpretation.Instance.EventInterpretation.AccessEvent;
				break;
			case EventType.Leave:
				interpretation = Interpretation.Instance.EventInterpretation.LeaveEvent;
				break;
			case EventType.Modify:
				interpretation = Interpretation.Instance.EventInterpretation.ModifyEvent;
				break;
			case EventType.Move:
				interpretation = Interpretation.Instance.EventInterpretation.MoveEvent;
				break;
			case EventType.Create:
				interpretation = Interpretation.Instance.EventInterpretation.CreateEvent;
				break;
			case EventType.Delete:
				interpretation = Interpretation.Instance.EventInterpretation.DeleteEvent;
				break;
			}

			Event ev = new Event ();

			ev.Id = 0;
			ev.Timestamp = DateTime.Now;
			ev.Interpretation = interpretation;
			ev.Manifestation = eventManifestation;
			ev.Actor = MonoDevelopUri;

			Subject sub = new Subject ();

			sub.Uri = "file://" + oldPath.FullPath;
			if(newPath != null)
				sub.CurrentUri = "file://" + newPath.FullPath;
			
			sub.Interpretation = subjectInterpretation;
			sub.Manifestation = subjectManifestation;
			
			FilePath path = newPath != null? newPath : oldPath;
			MonoDevelop.Core.LoggingService.LogInfo ("File Opened: {0}", path.FullPath);
			sub.Origin = path.FullPath.ParentDirectory;
			sub.MimeType = DesktopService.GetMimeTypeForUri (path.FullPath);
			sub.Text = path.FileName;
			sub.Storage = string.Empty;

			ev.Subjects.Add (sub);

			List<Event> listOfEvents = new List<Event> ();
			listOfEvents.Add (ev);

			client.InsertEvents (listOfEvents);
		}

		public void SendFilePath (FilePath filePath, EventType type)
		{
			SendFilePath(filePath, null, type);
		}
	}
}

