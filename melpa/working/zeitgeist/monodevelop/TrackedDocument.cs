//
//  TrackedDocument.cs
//
//  Author:
//       Patrick McEvoy <patrick@qmtech.net>
//
//  Copyright (c) 2011 QMTech.
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
using MonoDevelop.Core;

namespace MonoDevelop.Zeitgeist
{
	internal class TrackedDocument
	{
		public Document Document { get; set; }
		public FilePath FilePath { get; set; }

		ZeitgeistClient client;

		public TrackedDocument (ZeitgeistClient _client, Document doc)
		{
			client = _client;

			doc.Closed += HandleDocClosed;
			doc.Saved += HandleDocSaved;
			Document = doc;
			FilePath = new FilePath (doc.FileName);
		}

		void HandleDocSaved (object sender, EventArgs e)
		{
			MonoDevelop.Core.LoggingService.LogInfo ("Logging modify of {0} to Zeitgeist", Document.FileName.FileName);
			client.SendFilePath (FilePath, EventType.Modify);
		}

		void HandleDocClosed (object sender, EventArgs e)
		{
			MonoDevelop.Core.LoggingService.LogInfo ("Logging leave of {0} to Zeitgeist", FilePath.FileName);
			client.SendFilePath (FilePath, EventType.Leave);
		}

		public virtual void Dispose ()
		{
			if (Destroyed != null)
				Destroyed (this, EventArgs.Empty);
		}

		public event EventHandler Destroyed;

		public bool IsCurrent ()
		{
			if (Document == null)
				return false;
			return true;
		}
	}
}

