//
//  StartupHandler.cs
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
using Mono.Addins;
using MonoDevelop.Components.Commands;
using MonoDevelop.Ide.Extensions;
using MonoDevelop.Projects;
using MonoDevelop.Ide.Gui;
using System.Collections.Generic;
using MonoDevelop.Core;
using System.Linq;

namespace MonoDevelop.Zeitgeist
{
	[Extension ("/MonoDevelop/Ide/StartupHandlers", NodeName="Class")]
	public class StartupHandler: CommandHandler
	{
		ZeitgeistClient client;
		IList<TrackedDocument> documents = new List<TrackedDocument> ();

		public StartupHandler ()
		{
			client = new ZeitgeistClient ();
		}

		protected override void Run ()
		{
			// TODO Don't log files as opened until they before active through usage when opening a project/solu
			// IDEA: Auto-close tabs that aren't being used
			Ide.IdeApp.Workbench.DocumentOpened += HandleDocumentOpened;
			
			MonoDevelop.Core.FileService.FileCreated += HandleMonoDevelopCoreFileServiceFileCreated;
			MonoDevelop.Core.FileService.FileRenamed += HandleMonoDevelopCoreFileServiceFileRenamed;
			MonoDevelop.Core.FileService.FileRemoved += HandleMonoDevelopCoreFileServiceFileRemoved;
			
			Ide.IdeApp.Workspace.SolutionLoaded += HandleIdeIdeAppWorkspaceSolutionLoaded;
			Ide.IdeApp.Workspace.SolutionUnloaded += HandleIdeIdeAppWorkspaceSolutionUnloaded;
		}

		void HandleMonoDevelopCoreFileServiceFileCreated (object sender, FileEventArgs e)
		{
			foreach(var fileInfo in e.ToList())
			{
				if(!fileInfo.IsDirectory)
				{
					MonoDevelop.Core.LoggingService.LogInfo ("File {0} created", 
						fileInfo.FileName.FileName);
					client.SendFilePath (fileInfo.FileName, EventType.Create);
				}
			}
		}

		void HandleMonoDevelopCoreFileServiceFileRemoved (object sender, FileEventArgs e)
		{
			foreach(var fileInfo in e.ToList())
			{
				if(!fileInfo.IsDirectory)
				{
					MonoDevelop.Core.LoggingService.LogInfo ("File {0} deleted", 
						fileInfo.FileName.FileName);
					client.SendFilePath (fileInfo.FileName, EventType.Delete);
				}
			}
		}

		void HandleMonoDevelopCoreFileServiceFileRenamed (object sender, FileCopyEventArgs e)
		{
			foreach(FileCopyEventInfo evInfo in e.ToList())
			{
				if(!evInfo.IsDirectory)
				{
					MonoDevelop.Core.LoggingService.LogInfo ("Renamed {0} to {1}", 
						evInfo.SourceFile.FileName, evInfo.TargetFile.FileName);
					client.SendFilePath (evInfo.SourceFile, evInfo.TargetFile, EventType.Move);
				}
			}
		}

		void HandleIdeIdeAppWorkspaceSolutionUnloaded (object sender, SolutionEventArgs e)
		{
			MonoDevelop.Core.LoggingService.LogInfo ("Solution Unloaded: {0}", e.Solution.FileName.FileName);
			client.SendFilePath (e.Solution.FileName, EventType.Leave);
		}

		void HandleIdeIdeAppWorkspaceSolutionLoaded (object sender, SolutionEventArgs e)
		{
			MonoDevelop.Core.LoggingService.LogInfo ("Solution Loaded: {0}", e.Solution.FileName.FileName);
			client.SendFilePath (e.Solution.FileName, EventType.Access);
		}

		void HandleDocumentOpened (object sender, DocumentEventArgs e)
		{
			if (!e.Document.IsFile)
				return;

			MonoDevelop.Core.LoggingService.LogInfo ("Logging access of {0} to Zeitgeist", e.Document.FileName.FileName);
			var tracked = new TrackedDocument (client, e.Document);
			tracked.Destroyed += delegate {
				foreach (var t in documents)
					if (!t.IsCurrent ())
						documents.Remove (t);
			};
			documents.Add (tracked);
			client.SendFilePath (e.Document.FileName, EventType.Access);
		}
	}
}

